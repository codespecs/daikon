package daikon.suppress;

import static daikon.tools.nullness.NullnessUtil.castNonNullDeep;

import daikon.Debug;
import daikon.Global;
import daikon.PptTopLevel;
import daikon.VarInfo;
import daikon.inv.Invariant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.StringsPlume;

/**
 * Class that defines a single non-instantiating suppression. A suppression consists of one or more
 * suppressors and a suppressee. If each of the suppressors is true they imply the suppressee.
 */
public class NISuppression {

  /** Set of suppressor invariants. */
  NISuppressor[] suppressors;

  /** Suppressee invariant. */
  NISuppressee suppressee;

  private boolean debug = false;

  public NISuppression(NISuppressor[] suppressor_set, NISuppressee suppressee) {

    suppressors = suppressor_set;
    this.suppressee = suppressee;
  }

  /**
   * Creates a NISuppression.
   *
   * @param suppressor_set the suppressor set
   * @param suppressee the suppressee
   */
  public NISuppression(List<NISuppressor> suppressor_set, NISuppressee suppressee) {

    suppressors = suppressor_set.toArray(new NISuppressor[0]);
    this.suppressee = suppressee;
  }

  public NISuppression(NISuppressor sup1, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1}, suppressee);
  }

  public NISuppression(NISuppressor sup1, NISuppressor sup2, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2}, suppressee);
  }

  public NISuppression(
      NISuppressor sup1, NISuppressor sup2, NISuppressor sup3, NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3}, suppressee);
  }

  public NISuppression(
      NISuppressor sup1,
      NISuppressor sup2,
      NISuppressor sup3,
      NISuppressor sup4,
      NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4}, suppressee);
  }

  public NISuppression(
      NISuppressor sup1,
      NISuppressor sup2,
      NISuppressor sup3,
      NISuppressor sup4,
      NISuppressor sup5,
      NISuppressee suppressee) {

    this(new NISuppressor[] {sup1, sup2, sup3, sup4, sup5}, suppressee);
  }

  public Iterator<NISuppressor> suppressor_iterator() {
    return Arrays.<NISuppressor>asList(suppressors).iterator();
  }

  /**
   * Checks this suppression. Each suppressor is checked to see if it matches inv and if not,
   * whether or not it is valid (true). The results are saved in each suppressor. The suppressor
   * results are used later by {@link #invalidated()}.
   *
   * @param ppt program point in which to check suppression
   * @param vis variables over which to check suppression
   * @param inv falsified invariant (if any). Any suppressor that matches inv will be marked as
   *     NIS.SuppressState.MATCH
   * @return NIS.SuppressState.VALID if the suppression is valid, NIS.SuppressState.NONSENSICAL if
   *     one or more suppressors were nonsensical and the rest were valid, NIS.SuppressState.INVALID
   *     otherwise
   */
  public NIS.SuppressState check(PptTopLevel ppt, VarInfo[] vis, @Nullable Invariant inv) {

    NIS.SuppressState status = NIS.SuppressState.VALID;
    boolean set = false;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      NIS.SuppressState st = ssor.check(ppt, vis, inv);

      if (!set) {
        if (st == NIS.SuppressState.NONSENSICAL) {
          status = NIS.SuppressState.NONSENSICAL;
        } else if (st != NIS.SuppressState.VALID) {
          status = NIS.SuppressState.INVALID;
          if (st == NIS.SuppressState.INVALID) {
            return status;
          }
          // !valid in this case means invalid or match
          // If invalid, then stop immediately
          // otherwise, check state of the rest of the suppressors
          // This check is needed because we are reviving the
          // falsified method so match is now a valid status.
          set = true;
        }
      }
    }
    return status;
  }

  /**
   * Determines whether or not the falsified invariant previously passed to {@link
   * #check(PptTopLevel,VarInfo[],Invariant)} was the first suppressor to be falsified in this
   * suppression. If the falsified invariant is not involved in this suppression, then it can't have
   * been invalidated.
   */
  public boolean invalidated() {

    // We return true when every suppressor except the falsified
    // one is valid and at least one suppressor matches the falsified
    // invariant.  Note that match can be true on more than one
    // suppressor due to reflexive (x, x, x) invariants.  In this
    // code, the suppressor should never be nonsensical, since we should
    // have never looked at a slice with nonsensical variables.
    boolean inv_match = false;
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor ssor = suppressors[i];
      assert ssor.state != NIS.SuppressState.NONSENSICAL;
      if (ssor.state == NIS.SuppressState.MATCH) {
        inv_match = true;
      } else if (ssor.state != NIS.SuppressState.VALID) {
        return false;
      }
    }
    return inv_match;
  }

  /**
   * Finds all of the invariants that are suppressed by this suppression.
   *
   * @param suppressed_invs any invariants that are suppressed by the antecedent invariants in
   *     {@code ants} using this suppression are added to this set
   * @param ants antecedents organized by class
   */
  public void find_suppressed_invs(Set<NIS.SupInv> suppressed_invs, NIS.Antecedents ants) {

    // debug = suppressee.sup_class.getName().indexOf("NonZero") != -1;
    if (debug) {
      System.out.println("In find_suppressed_invs for " + this);
    }

    // Get the antecedents that match our suppressors.  Return if there are
    // no antecedents for a particular suppressor.
    List<Invariant>[] antecedents = antecedents_for_suppressors(ants);
    if (antecedents == null) {
      return;
    }

    // Recursively check each combination of possible antecedents that
    // match our suppressors for suppressions
    VarInfo vis[] = new VarInfo[suppressee.var_count];
    find_suppressed_invs(suppressed_invs, antecedents, vis, 0);

    if (debug) {
      System.out.println("  suppressed invariants: " + suppressed_invs);
    }
  }

  /**
   * Finds invariants that have become unsuppressed (one or more of their antecedent invariants is
   * falsified). The invariant may still be suppressed by a different suppression.
   *
   * @param unsuppressed_invs any invariants that are suppressed by the antecedent invariants in
   *     ants using this suppression are added to this set if one or more of the antecedents are
   *     falsified
   * @param ants antecedents organized by class
   */
  public void find_unsuppressed_invs(Set<NIS.SupInv> unsuppressed_invs, NIS.Antecedents ants) {

    // debug = suppressee.sup_class.getName().indexOf("SeqIntLessEqual") != -1;

    // Get the antecedents that match our suppressors.  Return if there are
    // no antecedents for a particular suppressor.
    List<Invariant>[] antecedents = antecedents_for_suppressors(ants);
    if (antecedents == null) {
      return;
    }

    int total_false_cnt = 0;
    for (int i = 0; i < antecedents.length; i++) {
      List<Invariant> a = antecedents[i];
      int false_cnt = 0;
      for (Invariant inv : a) {
        if (inv.is_false()) {
          false_cnt++;
        }
      }

      total_false_cnt += false_cnt;
    }

    if (total_false_cnt == 0) {
      return;
    }

    // Recursively check each combination of possible antecedents that
    // match our suppressors for suppressions
    VarInfo vis[] = new VarInfo[suppressee.var_count];
    // int old_size = unsuppressed_invs.size();
    Invariant[] cinvs = new Invariant[antecedents.length];
    find_unsuppressed_invs(unsuppressed_invs, antecedents, vis, 0, false, cinvs);
    if (debug) {
      System.out.println("  unsuppressed invariants: " + unsuppressed_invs);
    }
  }

  /**
   * Recursively finds suppressed invariants. The cross product of antecedents for each suppressor
   * are examined and each valid combination will yield an entry in suppressed_invs.
   *
   * @param unsuppressed_invs this set is updated with any invariants that used to be suppressed,
   *     but no longer are
   * @param antecedents array of antecedents per suppressor
   * @param vis current variables for the suppressed invariant As antecedents are chosen, their
   *     variables are placed into vis
   * @param idx current index into suppressors and antecedents
   * @see #find_unsuppressed_invs (Set, List, VarInfo[], int, boolean)
   * @see #consider_inv (Invariant, NISuppressor, VarInfo[])
   */
  private void find_suppressed_invs(
      Set<NIS.SupInv> unsuppressed_invs, List<Invariant> antecedents[], VarInfo vis[], int idx) {

    // Loop through each antecedent that matches the current suppressor
    NISuppressor s = suppressors[idx];
    for (Invariant inv : antecedents[idx]) {
      PptTopLevel ppt = inv.ppt.parent;
      assert ppt.equality_view != null : "@AssumeAssertion(nullness): need to check justification";

      // See if this antecedent can be used with the ones we have found so far
      VarInfo[] cvis = consider_inv(inv, s, vis);
      if (cvis == null) {
        continue;
      }

      // If this is the last suppressor
      if ((idx + 1) == suppressors.length) {

        // Create descriptions of the suppressed invariants
        List<NIS.SupInv> new_invs = suppressee.find_all(cvis, ppt, null);
        unsuppressed_invs.addAll(new_invs);

        // Check to insure that none of the invariants already exists
        if (Debug.dkconfig_internal_check) {
          for (NIS.SupInv supinv : new_invs) {
            Invariant cinv = supinv.already_exists();
            if (cinv != null) {
              @SuppressWarnings("nullness")
              @NonNull NISuppressionSet ss = cinv.get_ni_suppressions();
              // this is apparently called for side effect (debugging output)
              ss.suppressed(cinv.ppt);
              throw new Error(
                  "inv "
                      + cinv.repr()
                      + " of class "
                      + supinv.suppressee
                      + " already exists in ppt "
                      + ppt.name
                      + " suppressionset = "
                      + ss
                      + " suppression = "
                      + this
                      + " last antecedent = "
                      + inv.format());
            }
          }
        }
      } else {
        // Recursively process the next suppressor
        find_suppressed_invs(unsuppressed_invs, antecedents, cvis, idx + 1);
      }
    }
  }

  /**
   * Recursively finds unsuppressed invariants. The cross product of antecedents for each suppressor
   * is examined and each valid combination with at least one falsified antecedent will yield an
   * entry in unsuppressed_invs.
   *
   * @param unsuppressed_invs this set is updated with any invariants that were suppressed, but one
   *     of the suppressors is falsified (thus, the invariant is no longer suppressed)
   * @param antecedents array of antecedents per suppressor
   * @param vis current variables for the suppressed invariant As antecedents are chosen, their
   *     variables are placed into vis
   * @param idx current index into suppressors and antecedents
   * @param false_antecedents true if a false antecedent has been found
   * @param cinvs the invariants associated with the current set of antecedents. Used only for debug
   *     printing. May be side-effected by having cinvs[idx] set to null.
   * @see find_unsuppressed_invs (Set, List, VarInfo[], int)
   * @see #consider_inv (Invariant, NISuppressor, VarInfo[])
   */
  private void find_unsuppressed_invs(
      Set<NIS.SupInv> unsuppressed_invs,
      List<Invariant> antecedents[],
      VarInfo vis[],
      int idx,
      boolean false_antecedents,
      @Nullable Invariant[] cinvs) {

    boolean all_true_at_end = ((idx + 1) == suppressors.length) && !false_antecedents;

    // Loop through each antecedent that matches the current suppressor
    NISuppressor s = suppressors[idx];
    for (Invariant inv : antecedents[idx]) {
      PptTopLevel ppt = inv.ppt.parent;
      assert ppt.equality_view != null : "@AssumeAssertion(nullness): need to check justification";
      cinvs[idx] = inv;

      // If this is the last suppressor, no previous antecedents were
      // false, and this antecedent is not false either, we can stop
      // checking.  The antecedent lists are sorted so that the false
      // ones are first.  There is no need to look at antecedents that
      // are all true.
      if (all_true_at_end && !inv.is_false()) {
        cinvs[idx] = null;
        return;
      }
      // See if this antecedent can be used with the ones we have found so far
      VarInfo[] cvis = consider_inv(inv, s, vis);
      if (cvis == null) {
        continue;
      }

      // If this is the last suppressor
      if ((idx + 1) == suppressors.length) {

        // JHP: this check can be removed if the earlier check for all
        // true antecedents is included.
        if (!false_antecedents && !inv.is_false()) {
          if (debug) {
            System.out.printf("Skipping %s, no false antecedents%n", Arrays.toString(cvis));
          }
          continue;
        }

        // Create descriptions of the suppressed invariants
        List<NIS.SupInv> new_invs = suppressee.find_all(cvis, ppt, cinvs);
        if (debug) {
          System.out.printf("created %s new invariants", new_invs);
        }
        unsuppressed_invs.addAll(new_invs);

        // Check to insure that none of the invariants already exists
        if (Debug.dkconfig_internal_check) {
          for (NIS.SupInv supinv : new_invs) {
            Invariant cinv = supinv.already_exists();
            if (cinv != null) {
              throw new Error(
                  "inv "
                      + cinv.format()
                      + " of class "
                      + supinv.suppressee
                      + " already exists in ppt "
                      + ppt.name);
            }
          }
        }
      } else {
        // Recursively process the next suppressor
        find_unsuppressed_invs(
            unsuppressed_invs,
            antecedents,
            cvis,
            idx + 1,
            false_antecedents || inv.is_false(),
            cinvs);
      }
    }
    cinvs[idx] = null;
  }

  /**
   * Determine if the specified invariant can be used as part of this suppression. The invariant
   * must match suppressor and its variables must match up with any antecedents that have been
   * previously processed. As invariants are processed by this method, their variables are added to
   * the slots in vis that correspond to their suppressor.
   *
   * <p>For example, consider the invariant 'result = arg1 * arg2', the suppression '(result=arg1) ^
   * (arg2=1)' and the invariants 'x = y' and 'q = 1'. If the varinfo_index of 'q' is less than 'x'
   * then it can't be used (because it would form an invalid permutation. Note that this set of
   * antecedents will match a different suppression for multiply that has a different argument
   * permutation. More complex suppressions may refer to the same variable more than once. In those
   * cases, the antecedent invariants must also be over the same variables.
   *
   * @param inv the invariant to attempt to add to the suppression
   * @param supor the suppressor we are trying to match
   * @param vis the current variables (if any) that have already been determined by previous
   *     antecedents
   * @return a new VarInfo[] containing the variables of inv, or null if inv does not match in some
   *     way
   */
  private VarInfo @Nullable [] consider_inv(Invariant inv, NISuppressor supor, VarInfo[] vis) {

    // Make sure this invariant really matches this suppressor.  We know
    // the class already matches, but if the invariant has a swap variable
    // it must match as well
    if (!supor.match(inv)) {
      return null;
    }

    // Assign the variables from this invariant into vis.  If a variable
    // is already there and doesn't match this variable, then this
    // antecedent can't be used.
    VarInfo v1 = inv.ppt.var_infos[0];
    if ((vis[supor.v1_index] != null) && (vis[supor.v1_index] != v1)) {
      return null;
    }
    if ((supor.v2_index != -1)
        && (vis[supor.v2_index] != null)
        && (vis[supor.v2_index] != inv.ppt.var_infos[1])) {
      return null;
    }
    VarInfo cvis[] = vis.clone();
    cvis[supor.v1_index] = v1;
    if (supor.v2_index != -1) {
      cvis[supor.v2_index] = inv.ppt.var_infos[1];
    }
    if (debug) {
      System.out.printf(
          "Placed antecedent '%s' into cvis %s%n", inv.format(), Arrays.toString(cvis));
    }

    // Make sure the resulting variables are in the proper order and are
    // compatible
    if (!vis_order_ok(cvis) || !vis_compatible(cvis)) {
      if (debug) {
        System.out.println("Skipping, cvis has bad order or is incompatible");
      }
      return null;
    }

    return cvis;
  }

  /**
   * Builds an array of lists of antecedents that corresponds to each suppressor in this
   * suppression. Returns null if the list is empty for any suppressor (because that means there
   * can't be any suppressions based on these antecedents).
   */
  List<Invariant> @Nullable [] antecedents_for_suppressors(NIS.Antecedents ants) {

    @SuppressWarnings({"unchecked", "rawtypes"})
    /*NNC:@MonotonicNonNull*/ List<Invariant> antecedents[] =
        (List<Invariant>[]) new List[suppressors.length];

    // Find the list of antecedents that matches each suppressor.  If any
    // suppressor doesn't have any matching antecedents, there can't be
    // any invariants that are suppressed by this suppression.
    for (int i = 0; i < suppressors.length; i++) {
      NISuppressor s = suppressors[i];
      List<Invariant> alist = ants.get(s.get_inv_class());
      if (alist == null) {
        return null;
      }
      antecedents[i] = alist;
    }

    if (debug) {
      System.out.println(
          suppressee.sup_class.getName() + " " + antecedents_for_suppression(antecedents));
    }

    antecedents = castNonNullDeep(antecedents); // https://tinyurl.com/cfissue/986

    return antecedents;
  }

  /**
   * Determines whether the order of the variables in vis a valid permutations (i.e., their
   * varinfo_index's are ordered). Null elements are ignored (and an all-null list is OK).
   */
  private boolean vis_order_ok(VarInfo[] vis) {

    VarInfo prev = vis[0];
    for (int i = 1; i < vis.length; i++) {
      if ((prev != null) && (vis[i] != null)) {
        if (vis[i].varinfo_index < prev.varinfo_index) {
          return false;
        }
      }
      if (vis[i] != null) {
        prev = vis[i];
      }
    }
    return true;
  }

  /**
   * Determines if the non-null entries in vis are comparable. Returns true if they are, false if
   * they are not.
   *
   * <p>JHP: This should really be part of is_slice_ok.
   */
  public static boolean vis_compatible(VarInfo[] vis) {

    // Unary vis are always compatble
    if (vis.length == 1) {
      return true;
    }

    // Check binary
    if (vis.length == 2) {
      if ((vis[0] == null) || (vis[1] == null)) {
        return true;
      }

      if (vis[0].rep_type.isArray() == vis[1].rep_type.isArray()) {
        return vis[0].compatible(vis[1]);
      } else if (vis[0].rep_type.isArray()) {
        return vis[0].eltsCompatible(vis[1]);
      } else {
        return vis[1].eltsCompatible(vis[0]);
      }
    }

    // Check ternary
    if ((vis[1] != null) && (vis[2] != null)) {
      if (!vis[1].compatible(vis[2])) {
        return false;
      }
    }

    if ((vis[0] != null) && (vis[2] != null)) {
      if (!vis[0].compatible(vis[2])) {
        return false;
      }
    }

    return true;
  }

  public List<NISuppression> recurse_definition(NISuppressionSet ss) {

    NISuppressee sse = ss.get_suppressee();
    List<NISuppression> new_suppressions = new ArrayList<>();

    // Create a list of all of our suppressors that don't match the suppressee
    // of ss
    List<NISuppressor> old_sors = new ArrayList<>();
    NISuppressor match = null;
    for (int i = 0; i < suppressors.length; i++) {
      if (suppressors[i].match(sse)) {
        match = suppressors[i];
      } else {
        old_sors.add(suppressors[i]);
      }
    }

    // If we didn't match any suppressor there is nothing to do
    if (match == null) {
      return new_suppressions;
    }

    // Right now this only works if we match exactly one suppressor
    assert (old_sors.size() + 1) == suppressors.length;

    // Create one new suppression for each suppression in ss.  The suppressee
    // of ss is replaced by one of the suppressions of ss.  Each suppressor
    // in ss have its variable indices modified to match the original
    // suppressor.
    for (int i = 0; i < ss.suppression_set.length; i++) {
      NISuppression s = ss.suppression_set[i];
      List<NISuppressor> sors = new ArrayList<>(old_sors);
      for (int j = 0; j < s.suppressors.length; j++) {
        sors.add(s.suppressors[j].translate(match));
      }
      new_suppressions.add(new NISuppression(sors, suppressee));
    }

    return new_suppressions;
  }

  /** Clears the suppressor state in each suppressor. */
  public void clear_state() {
    for (int i = 0; i < suppressors.length; i++) {
      suppressors[i].clear_state();
    }
  }

  /** Returns {@code "suppressor && suppressor ... ==> suppressee"}. */
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied NISuppression this) {
    String suppressorsString =
        (suppressors.length == 1)
            ? suppressors[0].toString()
            : "(" + StringsPlume.join(" && ", suppressors) + ")";
    return suppressorsString + " ==> " + suppressee;
  }

  /** Returns a string describing each of the antecedents for each suppressor. */
  public String antecedents_for_suppression(List<Invariant> antecedents[]) {

    String sep = Global.lineSep;

    String out = "suppression " + this + sep;
    for (int i = 0; i < antecedents.length; i++) {
      out += "antecedents for suppressor " + i + sep;
      for (Invariant inv : antecedents[i]) {
        out += "    " + inv.format() + (inv.is_false() ? " [false]" : " t") + sep;
      }
    }
    return out;
  }
}
