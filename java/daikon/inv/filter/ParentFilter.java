package daikon.inv.filter;

import daikon.*;
import daikon.inv.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/** Filter for not printing invariants that have a matching invariant at their parent PPT. */
public class ParentFilter extends InvariantFilter {
  @Override
  public String getDescription() {
    return "Filter invariants that match a parent program point invariant";
  }

  /** Boolean. If true, ParentFilter is initially turned on. */
  public static boolean dkconfig_enabled = true;

  public ParentFilter() {
    isOn = dkconfig_enabled;
  }

  private static boolean debug = false;
  // private static boolean debug = true;

  @Override
  boolean shouldDiscardInvariant(Invariant inv) {

    // System.out.printf("shouldDiscardInvariant(%s)%n", inv.format());

    if (Debug.logDetail()) {
      if (inv.ppt.parent.parents != null) {
        inv.log(
            "%s has PptTopLevel %s which has %d parents",
            inv.format(), inv.ppt.parent.name, inv.ppt.parent.parents.size());
        for (PptRelation rel : inv.ppt.parent.parents) {
          inv.log("--%s%n", rel);
          inv.log("--variables: %s", VarInfo.arrayToString(rel.parent.var_infos));
          inv.log("--map: %s", rel.child_to_parent_map);
        }
      } else {
        inv.log("%s has PptTopLevel %s which has 0 parents", inv.format(), inv.ppt.parent.name);
      }
    }

    // If there are no parents, can't discard
    if (inv.ppt.parent.parents == null) return false;

    // Loop through each parent ppt getting the parent/child relation info
    outer:
    for (PptRelation rel : inv.ppt.parent.parents) {

      if (Debug.logDetail()) inv.log("  considering parent %s [%s]", rel, rel.parent.name());

      // Look up each variable in the parent, skip this parent if any
      // variables don't exist in the parent.
      /*@Nullable*/ VarInfo[] pvis_raw = new VarInfo[inv.ppt.var_infos.length];
      for (int j = 0; j < pvis_raw.length; j++) {
        pvis_raw[j] = rel.parentVar(inv.ppt.var_infos[j]);
        // pvis_raw[j] *can* be null.  Why, and is that a problem? -MDE
        if (pvis_raw[j] == null) {
          if (Debug.logDetail()) {
            inv.log(
                "variable %s [%s] cannot be found in %s",
                inv.ppt.var_infos[j], inv.ppt.var_infos[j].get_equalitySet_vars(), rel);
            for (VarInfo evi : inv.ppt.var_infos[j].get_equalitySet_vars()) {
              inv.log(
                  "var %s index %d, dp %b, depth %d, complex %d, idp %s, name %s, param vars %s",
                  evi,
                  evi.varinfo_index,
                  evi.isDerivedParamAndUninteresting(),
                  evi.derivedDepth(),
                  evi.complexity(),
                  evi.isDerivedParam(),
                  evi.get_VarInfoName(),
                  evi.ppt.getParamVars());
            }
          }
          continue outer;
        }
      }
      @SuppressWarnings("nullness") // at this point, pvis contains only non-null elements
      VarInfo[] pvis = pvis_raw;

      if (Debug.logDetail()) inv.log("  got variables");

      // Sort the parent variables in index order
      Arrays.sort(pvis, VarInfo.IndexComparator.getInstance());
      if (Debug.logDetail()) inv.log("Found parent vars: %s", VarInfo.arrayToString(pvis));

      // Lookup the slice, skip if not found
      PptSlice pslice = rel.parent.findSlice(pvis);
      if (pslice == null) {
        continue;
      }
      if (Debug.logDetail()) inv.log("Found parent slice: %s", pslice.name());

      // System.out.printf ("  found parent slice (%d invs): %s%n", pslice.invs.size(), pslice.name());

      // Look for a matching invariant in the parent slice.
      for (Invariant pinv : pslice.invs) {
        // System.out.printf ("  inv in parent slice: %s%n", pinv.format());
        if (pinv.isGuardingPredicate) {
          continue;
        }
        if (pinv.getClass() != inv.getClass()) {
          continue;
        }
        if (!pinv.isSameFormula(inv)) {
          continue;
        }

        // Check that all the guard variables correspond.
        List<VarInfo> guardedVars = inv.getGuardingList();
        List<VarInfo> pGuardedVars = pinv.getGuardingList();
        // Optimization: bail our early if size of list is different.
        if ((guardedVars.size() != pGuardedVars.size())
            && (guardedVars.size() != pGuardedVars.size() + 1)) {
          continue;
        }
        boolean var_mismatch = false;
        for (VarInfo v : guardedVars) {
          VarInfo pv = rel.parentVarAnyInEquality(v);
          // VarInfo pv = rel.parentVar(v);
          if (pv == null) {
            if (debug) {
              System.out.printf("    ParentFilter %s, parent %s%n", inv.format(), pslice.name());
              System.out.printf("    No parent var for %s via %s%n", v.name(), rel);
              System.out.printf("      Equality set: %s%n", v.equalitySet.shortString());
            }
            var_mismatch = true;
            break;
          }
          if (!(pv.name().equals("this") || pGuardedVars.contains(pv))) {
            if (debug) {
              System.out.printf(
                  "Not in guarding list %s for %s: parent var %s at %s for %s at %s%n",
                  guardedVars, pinv, pv, rel.parent, v.name(), rel.child);
            }
            VarInfo pgv = pGuardedVars.size() > 0 ? pGuardedVars.get(0) : null;
            assert (pgv != pv);
            if (debug && pgv != null) {
              System.out.printf(
                  "%s is index %d at %s, %s is index %d at %s%n",
                  pgv, pgv.varinfo_index, pgv.ppt.name, pv, pv.varinfo_index, pv.ppt.name);
            }
            var_mismatch = true;
            break;
          }
        }
        if (var_mismatch) {
          continue;
        }

        if (Invariant.logOn()) {
          inv.log(
              "Filtered by parent inv '%s' at ppt %s with rel %s",
              pinv.format(), pslice.name(), rel);
          for (VarInfo cvi : inv.ppt.var_infos) {
            inv.log("child variable %s matches parent variable %s", cvi, rel.parentVar(cvi));
            for (VarInfo evi : cvi.get_equalitySet_vars()) {
              inv.log(
                  "var %s index %d, dp %b, depth %d, complex %d",
                  evi,
                  evi.varinfo_index,
                  evi.isDerivedParamAndUninteresting(),
                  evi.derivedDepth(),
                  evi.complexity());
            }
          }
        }
        return true;
      }
    }

    return false;
  }
}
