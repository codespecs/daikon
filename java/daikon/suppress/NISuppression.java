package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.Logger;
import java.util.*;

/**
 * Class that defines a single non-instantiating suppression.  A suppression
 * consists of one or more suppressors and a suppressee.  If each of the
 * suppressors is true they imply the suppressee
 */
public class NISuppression {

  /** Set of suppressor invariants. **/
  NISuppressor[] suppressors;

  /** Suppressee invariant. **/
  NISuppressee suppressee;

  private boolean debug = false;

  public NISuppression (NISuppressor[] suppressor_set,
                        NISuppressee suppressee) {

    suppressors = suppressor_set;
    this.suppressee = suppressee;
  }

  public NISuppression (NISuppressor sup1, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressor sup4, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4}, suppressee);
  }

  public NISuppression (NISuppressor sup1, NISuppressor sup2, NISuppressor sup3,
                        NISuppressor sup4, NISuppressor sup5,
                        NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4, sup5}, suppressee);
  }

  public Iterator suppressor_iterator() {
    return Arrays.asList(suppressors).iterator();
  }

  /**
   * Checks this suppression.  Each suppressor is checked to see
   * if it matches inv and if not, whether or not it is valid.
   * The results are saved in each suppressor.  A null inv doesn't
   * match any suppressors
   *
   * @return VALID if the suppression is valid, MISSING if one or
   *         more suppressors were missing and the rest were valid,
   *         INVALID otherwise
   */
  public String check (PptTopLevel ppt, VarInfo[] vis, Invariant inv) {

    String status = NIS.VALID;
    boolean valid = true;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      String st = ssor.check (ppt, vis, inv);
      if (st == NIS.MISSING)
        status = NIS.MISSING;
      else if (st != NIS.VALID) {
        status = NIS.INVALID;
      }
    }
    return (status);
  }

  /**
   * Determines whether or not the falsified invariant was the
   * first suppressor to be falsified in this suppression.  This
   * depends on check() being called previously to fill in the state
   * for each suppressor.  If the falsified invariant is not involved
   * in this suppression, then it can't have been invalidated.
   */
  public boolean invalidated() {

    // We return true when every suppressor except the falsified
    // one is valid.  Note that match can be true on more than one
    // suppressor due to reflexive (x, x, x) invariants.  In this
    // code, the suppressor should never be missing, since we should
    // have never looked at a slice with missing variables.
    boolean inv_match = false;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      Assert.assertTrue (ssor.state != NIS.MISSING);
      if (ssor.state == NIS.MATCH) {
        // Assert.assertTrue (!inv_match);
        inv_match = true;
      } else if (ssor.state != NIS.VALID)
        return (false);
    }
    return (inv_match);
  }

  /**
   * Creates all of the invariants that are suppressed by this
   * suppression.
   *
   * @param antecedent_map  Map of all possible antecedents indexed by
   *                        their class.
   *
   * @return the list of created invariants.
   */
  public List/*Invariant*/ create_suppressed_invs
                    (Map/*Invariant.class->List<Invariant>*/antecedent_map) {

    // debug = suppressee.sup_class.getName().indexOf("mumDouble_xyz") != -1;

    VarInfo vis[] = new VarInfo[3];
    List/*Invariant*/ antecedents[] = new List [suppressors.length];

    // Find the list of antecedents that matches each suppressor.  If any
    // suppressor doesn't have any matching antecedents, there can't be
    // any invariants that are suppressed by this suppression.
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor s = suppressors[i];
      List/*Invariant*/ alist = (List) antecedent_map.get (s.get_inv_class());
      if (alist == null)
        return (new ArrayList());
      antecedents[i] = alist;
    }

    // List of suppressed invariants (to be returned)
    List/*Invariant*/ suppressed_invs = new ArrayList (100);

    if (debug)
      Fmt.pf (suppressee.sup_class.getName() + " " +
              antecedents_for_suppression (antecedents));

    // Check each combination of possible antecedents that match our
    // suppressors for suppressions
    create_suppressed_invs (suppressed_invs, antecedents, vis, 0);

    if (debug)
      Fmt.pf ("  suppressed invariants: " + suppressed_invs);

    return (suppressed_invs);
  }

  /**
   * Creates all of the invariants that are suppressed by this
   * suppression.
   *
   * @param suppressed_invs     This set is updated with any suppressed
   *                            invariants
   * @param unsuppressed_invs   This set is updated with any invariants
   *                            that are suppressed, but one of the
   *                            suppressors is falsified (thus, the invariant
   *                            is no longer suppressed)
   * @param antecedent_map      Map of all possible antecedents indexed by
   *                            their class.
   *
   * @return the list of created invariants.
   */
  public void find_suppressed_invs
                    (Set/*SupInv*/ suppressed_invs,
                     Set/*SupInv*/ unsuppressed_invs,
                     Map/*Invariant.class->List<Invariant>*/antecedent_map) {

    debug = suppressee.sup_class.getName().indexOf("Lshift_xyz") != -1;

    VarInfo vis[] = new VarInfo[3];
    List/*Invariant*/ antecedents[] = new List [suppressors.length];

    // Find the list of antecedents that matches each suppressor.  If any
    // suppressor doesn't have any matching antecedents, there can't be
    // any invariants that are suppressed by this suppression.
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor s = suppressors[i];
      List/*Invariant*/ alist = (List) antecedent_map.get (s.get_inv_class());
      if (alist == null)
        return;
      antecedents[i] = alist;
    }

    if (debug)
      Fmt.pf (suppressee.sup_class.getName() + " " +
              antecedents_for_suppression (antecedents));

    // Check each combination of possible antecedents that match our
    // suppressors for suppressions
    create_suppressed_invs (suppressed_invs, unsuppressed_invs, antecedents,
                            vis, 0, false);

    if (debug)
      Fmt.pf ("  suppressed invariants: " + suppressed_invs);

  }

  /**
   * Recursively creates suppressed invariants.  All possible
   * suppressions over the antecedents are examined (cross product
   * of the antecedents for each suppressor).  Since suppressors
   * refer to specific variables in the suppressee, only some
   * combinations of antecedents will be reasonable.
   *
   * For example, consider the invariant 'result = arg1 * arg2',
   * the suppression '(result=arg1) ^ (arg2=1)' and the invariants
   * 'x = y' and 'q = 1'.  If the varinfo_index of 'q' is less than
   * 'x' then it can't be used (because it would form an invalid
   * permutation.  Note that this set of antecedents will match
   * a different suppression for multiply that has a different
   * argument permutation.  More complex suppressions may refer
   * to the same variable more than once.  In those cases, the
   * natecedent invariants must also be over the same variables.
   *
   * @param suppressed_invs     This list is updated with any suppressed
   *                            invariants
   * @param antecedents         Array of antecedents per suppressor
   * @param vis                 Current variables for the suppressed invariant
   *                            As antecedents are chosen, their variables
   *                            are placed into vis.
   * @param idx                 Current index into suppressors and antecedents
   */
  private void create_suppressed_invs (List/*Invariant*/suppressed_invs,
                                       List/*Invariant*/antecedents[],
                                       VarInfo vis[], int idx) {

    NISuppressor s = suppressors[idx];
    for (Iterator i = antecedents[idx].iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      VarInfo cvis[] = (VarInfo[]) vis.clone();

      PptTopLevel ppt = inv.ppt.parent;
      VarInfo v1 = inv.ppt.var_infos[0];

      // Make sure this invariant really matches this suppressor.  We know
      // the class already matches, but if the invariant has a swap variable
      // it must match as well
      if (!s.match (inv))
        continue;

      // Assign the variables from this invariant into vis.  If a variable
      // is already there and doesn't match this variable, then this
      // antecedent can't be used.
      if ((cvis[s.v1_index] != null) && (cvis[s.v1_index] != v1))
        continue;
      cvis[s.v1_index] = v1;
      if (s.v2_index != -1) {
        VarInfo v2 = inv.ppt.var_infos[1];
        if ((cvis[s.v2_index] != null) && (cvis[s.v2_index] != v2))
          continue;
        cvis[s.v2_index] = v2;
      }
      if (debug)
        Fmt.pf ("Placed antecedent '%s' into cvis %s", inv.format(),
                VarInfo.toString(cvis));

      // Make sure the resulting variables are in the proper order
      if (!ppt.vis_order_ok (cvis)) {
        if (debug)
          Fmt.pf ("Skipping, cvis has bad order");
        continue;
      }

      // If this is the last suppressor, create the invariant
      if ((idx + 1) == suppressors.length) {
        List new_invs = suppressee.instantiate_all (cvis, ppt);
        suppressed_invs.addAll (new_invs);
        if (Daikon.dkconfig_internal_check) {
          if (debug && new_invs.size() == 0)
            Fmt.pf ("No %s invariants created over %s", suppressee.sup_class,
                    VarInfo.toString (cvis));
          for (Iterator j = new_invs.iterator(); j.hasNext(); ) {
            Invariant new_inv = (Invariant) j.next();
            if (debug)
              Fmt.pf ("Created new inv: " + new_inv.format());
            if (new_inv.ppt.find_inv_exact (new_inv) != null)
              Assert.assertTrue (false, "inv " + new_inv.format()
                                 + " already exists in ppt " + ppt.name
                                 + Global.lineSep + " last inv vars = "
                                 + VarInfo.toString(inv.ppt.var_infos)
                                 + " v1_index=" + s.v1_index
                                 + " v2_index=" + s.v2_index
                                 + antecedents_for_suppression (antecedents));

          }
        }
        continue;
      }

      // Recursively process the next suppressor
      create_suppressed_invs (suppressed_invs, antecedents, cvis, idx + 1);
    }
  }
  /**
   * Recursively creates suppressed invariants.  All possible
   * suppressions over the antecedents are examined (cross product
   * of the antecedents for each suppressor).  Since suppressors
   * refer to specific variables in the suppressee, only some
   * combinations of antecedents will be reasonable.
   *
   * For example, consider the invariant 'result = arg1 * arg2',
   * the suppression '(result=arg1) ^ (arg2=1)' and the invariants
   * 'x = y' and 'q = 1'.  If the varinfo_index of 'q' is less than
   * 'x' then it can't be used (because it would form an invalid
   * permutation.  Note that this set of antecedents will match
   * a different suppression for multiply that has a different
   * argument permutation.  More complex suppressions may refer
   * to the same variable more than once.  In those cases, the
   * natecedent invariants must also be over the same variables.
   *
   * @param suppressed_invs     This set is updated with any suppressed
   *                            invariants
   * @param unsuppressed_invs   This set is updated with any invariants
   *                            that are suppressed, but one of the
   *                            suppressors is falsified (thus, the invariant
   *                            is no longer suppressed)
   * @param antecedents         Array of antecedents per suppressor
   * @param vis                 Current variables for the suppressed invariant
   *                            As antecedents are chosen, their variables
   *                            are placed into vis.
   * @param idx                 Current index into suppressors and antecedents
   * @param false_antecedents   True if a false antecedent has been found
   */
  private void create_suppressed_invs (Set/*SupInv*/ suppressed_invs,
                                       Set/*SupInv*/ unsuppressed_invs,
                                       List/*Invariant*/antecedents[],
                                       VarInfo vis[], int idx,
                                       boolean false_antecedents) {

    NISuppressor s = suppressors[idx];
    for (Iterator i = antecedents[idx].iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      VarInfo cvis[] = (VarInfo[]) vis.clone();

      PptTopLevel ppt = inv.ppt.parent;
      VarInfo v1 = inv.ppt.var_infos[0];

      // Make sure this invariant really matches this suppressor.  We know
      // the class already matches, but if the invariant has a swap variable
      // it must match as well
      if (!s.match (inv))
        continue;

      // Assign the variables from this invariant into vis.  If a variable
      // is already there and doesn't match this variable, then this
      // antecedent can't be used.
      if ((cvis[s.v1_index] != null) && (cvis[s.v1_index] != v1))
        continue;
      cvis[s.v1_index] = v1;
      if (s.v2_index != -1) {
        VarInfo v2 = inv.ppt.var_infos[1];
        if ((cvis[s.v2_index] != null) && (cvis[s.v2_index] != v2))
          continue;
        cvis[s.v2_index] = v2;
      }
      if (debug)
        Fmt.pf ("Placed antecedent '%s' into cvis %s", inv.format(),
                VarInfo.toString(cvis));

      // Make sure the resulting variables are in the proper order and are
      // compatible
      if (!ppt.vis_order_ok (cvis)) {
        if (debug)
          Fmt.pf ("Skipping, cvis has bad order");
        continue;
      }
      if (!vis_compatible (cvis))
        continue;

      // If this is the last suppressor, create the invariant
      if ((idx + 1) == suppressors.length) {
        List /*SupInv*/ new_invs = suppressee.find_all (cvis, ppt);
        if (false_antecedents || inv.is_false())
          unsuppressed_invs.addAll (new_invs);
        else
          suppressed_invs.addAll (new_invs);
        if (Daikon.dkconfig_internal_check) {
          for (Iterator j = new_invs.iterator(); j.hasNext(); ) {
            NIS.SupInv supinv = (NIS.SupInv) j.next();
            Invariant cinv = ppt.find_inv_by_class (supinv.vis,
                                                supinv.suppressee.sup_class);
            if (cinv != null)
              Assert.assertTrue (false, "inv " + cinv.format() + " of class "
                                 + supinv.suppressee
                                 + " already exists in ppt " + ppt.name);

          }
        }
        continue;
      }

      // Recursively process the next suppressor
      create_suppressed_invs (suppressed_invs, unsuppressed_invs, antecedents,
                              cvis, idx + 1,
                              false_antecedents || inv.is_false());
    }
  }

  /**
   * Determines whether the order of the variables in vis a valid
   * permutations (ie, their varinfo_index's are ordered).  Null
   * elements are ignored (and an all-null list is ok)
   */
  private boolean vis_order_ok (VarInfo[] vis) {

    VarInfo prev = vis[0];
    for (int i = 1; i < vis.length; i++) {
      if ((prev != null) && (vis[i] != null)) {
        if (vis[i].varinfo_index < prev.varinfo_index)
          return (false);
      }
      if (vis[i] != null)
        prev = vis[i];
    }
    return (true);
  }

  /**
   * Determines if the non-null entries in vis are comparable.  Returns
   * true if they are, false if they are not.
   */
  private boolean vis_compatible (VarInfo[] vis) {

    if ((vis[0] != null) && (vis[1] != null))
      if (!vis[0].compatible (vis[1]))
        return (false);

    if ((vis[1] != null) && (vis[2] != null))
      if (!vis[1].compatible (vis[2]))
        return (false);

    if ((vis[0] != null) && (vis[2] != null))
      if (!vis[0].compatible (vis[2]))
        return (false);

    return (true);
  }
  /**
   * Clears the suppressor state in each suppressor.
   */
  public void clear_state () {
    for (int i = 0; i < suppressors.length; i++) {
      suppressors[i].clear_state();
    }
  }

  /**
   * Returns 'suppressor && suppressor ... => suppressee'
   */
  public String toString() {
    return (UtilMDE.join(suppressors, " && ") + " ==> " + suppressee);
  }

  public String antecedents_for_suppression (List/*Invariant*/antecedents[]) {

    String sep = Global.lineSep;

    String out = "suppression " + this + sep;
    for (int i = 0; i < antecedents.length; i++) {
      out += "antecedents for suppressor " + i + sep;
      for (Iterator j = antecedents[i].iterator(); j.hasNext(); ) {
        Invariant inv = (Invariant) j.next();
        out += "    " + inv.format() + sep;
      }
    }
    return (out);
  }


}
