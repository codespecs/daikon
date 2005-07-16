package daikon.inv.filter;

import daikon.inv.*;
import daikon.*;
import java.util.*;

/**
 * Filter for not printing invariants that have a matching invariant
 * at their parent PPT.
 **/
public class ParentFilter extends InvariantFilter {
  public String getDescription() {
    return "Filter invariants that match a parent program point invariant";
  }

  /**
   * Boolean. If true, ParentFilter is initially turned on.
   */
  public static boolean dkconfig_enabled = true;

  public ParentFilter () {
    isOn = dkconfig_enabled;
  }

  boolean shouldDiscardInvariant( Invariant inv ) {

    // If there are no parents, can't discard
    if (inv.ppt.parent.parents == null)
      return (false);

    // Loop through each parent ppt
    outer: for (int i = 0; i < inv.ppt.parent.parents.size(); i++) {

      // Get the parent/child relation information
      PptRelation rel = (PptRelation) inv.ppt.parent.parents.get (i);

      // Look up each variable in the parent, skip this parent if any
      // variables don't exist in the parent.
      VarInfo[] pvis = new VarInfo[inv.ppt.var_infos.length];
      for (int j = 0; j < pvis.length; j++) {
        pvis[j] = rel.parentVar (inv.ppt.var_infos[j]);
        if (pvis[j] == null)
          continue outer;
      }

      // Sort the parent variables in index order
      Arrays.sort (pvis, VarInfo.IndexComparator.getInstance());
      if (Debug.logDetail())
        inv.log ("Found parent vars: " + VarInfo.toString (pvis));

      // Lookup the slice, skip if not found
      PptSlice pslice = rel.parent.findSlice (pvis);
      if (pslice == null)
        continue;
      if (Debug.logDetail())
        inv.log ("Found parent slice: " + pslice.name());

      // Look for a matching invariant in the parent slice
      for (int j = 0; j < pslice.invs.size(); j++) {
        Invariant pinv = pslice.invs.get (j);
        if (pinv.getClass() != inv.getClass())
          continue;
        if (pinv.isSameFormula (inv)) {
          inv.log ("Filtered by parent inv '" + pinv.format() + "' at ppt "
                   + pslice.name());
          return (true);
        }
      }
    }

    return (false);
  }

}
