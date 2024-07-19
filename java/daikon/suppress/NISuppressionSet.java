package daikon.suppress;

import static daikon.tools.nullness.NullnessUtil.castNonNullDeep;

import daikon.Debug;
import daikon.PptSlice;
import daikon.PptTopLevel;
import daikon.VarInfo;
import daikon.inv.Invariant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.StringsPlume;

/**
 * Class that defines a set of non-instantiating suppressions for a single invariant (suppressee).
 * Each of the suppressors individually implies the suppressee.
 *
 * <p>Not immutable: see recurse_definitions().
 */
public class NISuppressionSet implements Iterable<NISuppression> {

  public static final Logger debug = Logger.getLogger("daikon.suppress.NISuppressionSet");

  NISuppression[] suppression_set;

  public NISuppressionSet(NISuppression[] suppressions) {
    assert suppressions != null;
    assert suppressions.length != 0;
    suppression_set = suppressions;
  }

  @Override
  public Iterator<NISuppression> iterator() {
    List<NISuppression> asList = Arrays.<NISuppression>asList(suppression_set);
    return asList.iterator();
  }

  /**
   * Adds this set to the suppressor map. The map is from the class of the suppressor to this. If
   * the same suppressor class appears more than once, the suppression is only added once.
   */
  public void add_to_suppressor_map(
      Map<Class<? extends Invariant>, List<NISuppressionSet>> suppressor_map) {

    Set<Class<? extends Invariant>> all_suppressors =
        new LinkedHashSet<Class<? extends Invariant>>();

    // Loop through each suppression in the suppression set
    for (int i = 0; i < suppression_set.length; i++) {
      NISuppression suppression = suppression_set[i];

      // Loop through each suppressor in the suppression
      for (Iterator<NISuppressor> j = suppression.suppressor_iterator(); j.hasNext(); ) {
        NISuppressor suppressor = j.next();

        // If we have seen this suppressor already, skip it
        if (all_suppressors.contains(suppressor.get_inv_class())) {
          continue;
        }

        // Note that we have now seen this suppressor invariant class
        all_suppressors.add(suppressor.get_inv_class());

        // Get the list of suppression sets for this suppressor.  Create it
        // if this is the first one.  Add this set to the list
        List<NISuppressionSet> suppression_set_list =
            suppressor_map.computeIfAbsent(
                suppressor.get_inv_class(), __ -> new ArrayList<NISuppressionSet>());
        suppression_set_list.add(this);
      }
    }
  }

  /**
   * NIS process a falsified invariant. This method should be called for each falsified invariant in
   * turn. Any invariants for which inv is the last valid suppressor are added to new_invs.
   *
   * <p>Note, this is no longer the preferred approach, but is kept for informational purposes. Use
   * NIS.process_falsified_invs() instead.
   */
  public void falsified(Invariant inv, List<Invariant> new_invs) {

    // Get the ppt we are working in
    PptTopLevel ppt = inv.ppt.parent;
    assert ppt.equality_view != null
        : "@AssumeAssertion(nullness): haven't reasoned through the reason";

    // For now all suppressors are unary/binary and
    // all suppressees are unary, binary or ternary
    assert inv.ppt.var_infos.length < 3;

    // check unary, binary and ternary suppressees separately

    // unary suppressee
    if (suppression_set[0].suppressee.var_count == 1) {
      // Create all of the valid unary slices that use the vars from inv
      // and check to see if the invariant should be created for each slice
      if (inv.ppt.var_infos.length == 1) {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo[] vis = new VarInfo[] {v1};

        // Make sure the slice is interesting and has valid types over the
        // suppressee invariant
        if (!v1.missingOutOfBounds() && ppt.is_slice_ok(v1)) {
          if (suppression_set[0].suppressee.sample_inv.valid_types(vis)) {
            check_falsified(ppt, vis, inv, new_invs);
          }
        }
      }
      return;
    }

    // binary suppressee
    if (suppression_set[0].suppressee.var_count == 2) {
      // Create all of the valid binary slices that use the vars from inv
      // and check to see if the invariant should be created for each slice
      if (inv.ppt.var_infos.length == 2) {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo v2 = inv.ppt.var_infos[1];
        VarInfo[] vis = new VarInfo[] {v1, v2};

        // Make sure the slice is interesting and has valid types over the
        // suppressee invariant
        if (!v1.missingOutOfBounds() && !v2.missingOutOfBounds() && ppt.is_slice_ok(v1, v2)) {
          if (suppression_set[0].suppressee.sample_inv.valid_types(vis)) {
            check_falsified(ppt, vis, inv, new_invs);
          }
        }

      } else /* must be unary */ {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo[] leaders = ppt.equality_view.get_leaders_sorted();
        for (int i = 0; i < leaders.length; i++) {
          VarInfo l1 = leaders[i];

          // hashcode types are not involved in suppressions
          if (NIS.dkconfig_skip_hashcode_type) {
            if (l1.file_rep_type.isHashcode()) {
              continue;
            }
          }

          // Make sure the slice is interesting
          if (v1.missingOutOfBounds() || l1.missingOutOfBounds()) {
            continue;
          }
          if (!ppt.is_slice_ok(v1, l1)) {
            continue;
          }

          VarInfo[] vis;

          // Sort the variables
          if (v1.varinfo_index <= l1.varinfo_index) {
            vis = new VarInfo[] {v1, l1};
          } else {
            vis = new VarInfo[] {l1, v1};
          }

          if (!suppression_set[0].suppressee.sample_inv.valid_types(vis)) {
            continue;
          }

          if (NIS.debug.isLoggable(Level.FINE)) {
            NIS.debug.fine(
                "processing slice "
                    + Arrays.toString(vis)
                    + " in ppt "
                    + ppt.name()
                    + " with "
                    + ppt.numViews());
          }

          check_falsified(ppt, vis, inv, new_invs);
        }
      }
      return;
    }

    // ternary suppressee
    if (suppression_set[0].suppressee.var_count == 3) {
      // Create all of the valid ternary slices that use the vars from inv
      // and check to see if the invariant should be created for each slice
      if (inv.ppt.var_infos.length == 2) {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo v2 = inv.ppt.var_infos[1];
        VarInfo[] leaders = ppt.equality_view.get_leaders_sorted();
        for (int i = 0; i < leaders.length; i++) {
          VarInfo l = leaders[i];

          if (NIS.dkconfig_skip_hashcode_type) {
            if (l.file_rep_type.isHashcode()) {
              continue;
            }
          }

          if (!ppt.is_slice_ok(l, v1, v2)) {
            continue;
          }
          if (l.missingOutOfBounds() || v1.missingOutOfBounds() || v2.missingOutOfBounds()) {
            continue;
          }

          VarInfo[] vis;

          // Order the variables,
          if (l.varinfo_index <= v1.varinfo_index) {
            vis = new VarInfo[] {l, v1, v2};
          } else if (l.varinfo_index <= v2.varinfo_index) {
            vis = new VarInfo[] {v1, l, v2};
          } else {
            vis = new VarInfo[] {v1, v2, l};
          }

          if (!suppression_set[0].suppressee.sample_inv.valid_types(vis)) {
            continue;
          }

          if (NIS.debug.isLoggable(Level.FINE)) {
            NIS.debug.fine(
                "processing slice "
                    + Arrays.toString(vis)
                    + " in ppt "
                    + ppt.name()
                    + " with "
                    + ppt.numViews());
          }

          check_falsified(ppt, vis, inv, new_invs);
        }
      } else /* must be unary */ {
        VarInfo v1 = inv.ppt.var_infos[0];
        VarInfo[] leaders = ppt.equality_view.get_leaders_sorted();
        for (int i = 0; i < leaders.length; i++) {
          VarInfo l1 = leaders[i];

          if (NIS.dkconfig_skip_hashcode_type) {
            if (l1.file_rep_type.isHashcode()) {
              continue;
            }
          }

          for (int j = i; j < leaders.length; j++) {
            VarInfo l2 = leaders[j];

            if (NIS.dkconfig_skip_hashcode_type) {
              if (l2.file_rep_type.isHashcode()) {
                continue;
              }
            }

            // Make sure the slice is interesting
            if (v1.missingOutOfBounds() || l1.missingOutOfBounds() || l2.missingOutOfBounds()) {
              continue;
            }
            if (!ppt.is_slice_ok(v1, l1, l2)) {
              continue;
            }

            VarInfo[] vis;

            // Sort the variables
            if (v1.varinfo_index <= l1.varinfo_index) {
              vis = new VarInfo[] {v1, l1, l2};
            } else if (v1.varinfo_index <= l2.varinfo_index) {
              vis = new VarInfo[] {l1, v1, l2};
            } else {
              vis = new VarInfo[] {l1, l2, v1};
            }

            if (!suppression_set[0].suppressee.sample_inv.valid_types(vis)) {
              continue;
            }

            if (NIS.debug.isLoggable(Level.FINE)) {
              NIS.debug.fine(
                  "processing slice "
                      + Arrays.toString(vis)
                      + " in ppt "
                      + ppt.name()
                      + " with "
                      + ppt.numViews());
            }

            check_falsified(ppt, vis, inv, new_invs);
          }
        }
      }
      return;
    }
  }

  /**
   * Checks the falsified invariant against the slice specified by vis. If the falsification of inv
   * removed the last valid suppression, then instantiates the suppressee.
   */
  private void check_falsified(
      PptTopLevel ppt, VarInfo[] vis, Invariant inv, List<Invariant> new_invs) {

    // process each suppression in the set, marking each suppressor as
    // to whether it is true, false, or matches the falsified inv
    // If any particular suppression is still valid, just return as there
    // is nothing to be done (the suppressee is still suppressed)

    for (int i = 0; i < suppression_set.length; i++) {

      NIS.SuppressState status = suppression_set[i].check(ppt, vis, inv);
      if (status == NIS.SuppressState.VALID) {
        if (NIS.debug.isLoggable(Level.FINE)) {
          NIS.debug.fine("suppression " + suppression_set[i] + " is valid");
        }
        return;
      }
      assert status != NIS.SuppressState.NONSENSICAL;
    }

    if (NIS.debug.isLoggable(Level.FINE)) {
      NIS.debug.fine("After check, suppression set: " + this);
    }

    // There are no remaining valid (true) suppressions.  If inv is the
    // first suppressor to be removed from any suppressions, then this
    // falsification removed the last valid suppression.  In that case we
    // need to instantiate the suppressee.
    for (int i = 0; i < suppression_set.length; i++) {
      if (suppression_set[i].invalidated()) {

        Invariant v = suppression_set[i].suppressee.instantiate(vis, ppt);
        if (v != null) {
          new_invs.add(v);
        }
        return;
      }
    }
  }

  /**
   * Determines whether or not the suppression set is valid in the specified slice. The suppression
   * set is valid if any of its suppressions are valid. A suppression is valid if all of its
   * suppressors are true.
   *
   * <p>Also updates the debug information in each suppressor.
   *
   * @see #is_instantiate_ok(PptSlice) for a check that considers missing
   */
  public boolean suppressed(PptSlice slice) {

    return suppressed(slice.parent, slice.var_infos);
  }

  /**
   * Determines whether or not the suppression set is valid in the specified ppt and var_infos. The
   * suppression set is valid if any of its suppressions are valid. A suppression is valid if all of
   * its suppressors are true.
   *
   * <p>Also updates the debug information in each suppressor.
   *
   * @see #is_instantiate_ok(PptTopLevel,VarInfo[]) for a check that considers missing
   */
  public boolean suppressed(PptTopLevel ppt, VarInfo[] var_infos) {

    // Check each suppression to see if it is valid
    for (int i = 0; i < suppression_set.length; i++) {
      NIS.SuppressState status = suppression_set[i].check(ppt, var_infos, null);
      if (status == NIS.SuppressState.VALID) {
        if (Debug.logOn() || NIS.debug.isLoggable(Level.FINE)) {
          Debug.log(
              NIS.debug,
              getClass(),
              ppt,
              var_infos,
              "suppression "
                  + suppression_set[i]
                  + " is "
                  + status
                  + " in ppt "
                  + ppt
                  + " with var infos "
                  + Arrays.toString(var_infos));
        }
        return true;
      }
    }

    if (Debug.logOn() || NIS.debug.isLoggable(Level.FINE)) {
      Debug.log(
          NIS.debug,
          getClass(),
          ppt,
          var_infos,
          "suppression "
              + this
              + " is not valid in ppt "
              + ppt
              + " with var infos "
              + Arrays.toString(var_infos));
    }
    return false;
  }

  /**
   * Determines whether or not the suppression set is valid in the specified slice. The suppression
   * set is valid if any of its suppressions are valid. A suppression is valid if all of its
   * non-missing suppressors are true.
   */
  @Pure
  public boolean is_instantiate_ok(PptSlice slice) {

    return is_instantiate_ok(slice.parent, slice.var_infos);
  }

  /**
   * Determines whether or not the suppressee of the suppression set should be instantiated.
   * Instantiation is ok only if each suppression is invalid. A suppression is valid if all of its
   * non-missing suppressors are true.
   */
  @Pure
  public boolean is_instantiate_ok(PptTopLevel ppt, VarInfo[] var_infos) {

    // Check each suppression to see if it is valid
    for (int i = 0; i < suppression_set.length; i++) {
      NIS.SuppressState status = suppression_set[i].check(ppt, var_infos, null);
      if ((status == NIS.SuppressState.VALID) || (status == NIS.SuppressState.NONSENSICAL)) {
        if (Debug.logOn() || NIS.debug.isLoggable(Level.FINE)) {
          Debug.log(
              NIS.debug,
              getClass(),
              ppt,
              var_infos,
              "suppression "
                  + suppression_set[i]
                  + " is "
                  + status
                  + " in ppt "
                  + ppt
                  + " with var infos "
                  + Arrays.toString(var_infos));
        }
        return false;
      }
    }

    if (Debug.logOn() || NIS.debug.isLoggable(Level.FINE)) {
      Debug.log(
          NIS.debug,
          getClass(),
          ppt,
          var_infos,
          "suppression "
              + this
              + " is not valid in ppt "
              + ppt
              + " with var infos "
              + Arrays.toString(var_infos));
    }
    return true;
  }

  /**
   * Side-effects this NISuppressionSet. Each suppression where a suppressor matches the suppressee
   * in ss is augmented by additional suppression(s) where the suppressor is replaced by each of its
   * suppressions. This allows recursive suppressions.
   *
   * <p>For example, consider the suppressions:
   *
   * <pre>
   *    (r == arg1) &and; (arg2 &le; arg1) &rArr; r = max(arg1,arg2)
   *    (arg2 == arg1) &rArr; arg2 &le; arg1
   * </pre>
   *
   * The suppressor (arg2 &le; arg1) in the first suppression matches the suppressee in the second
   * suppression. In order for the first suppression to work even when (arg2 &le; arg1) is
   * suppressed, the second suppression is added to the first:
   *
   * <pre>
   *    (r == arg1) &and; (arg2 &le; arg1) &rArr; r = max(arg1,arg2)
   *    (r == arg1) &and; (arg2 == arg1) &rArr; r = max(arg1,arg2)
   * </pre>
   *
   * When (arg2 &le; arg1) is suppressed, the second suppression for max will still suppress max. If
   * (arg2 == arg1) is falsified, the (arg2 &le; arg1) invariant will be created and can continue to
   * suppress max (as long as it is not falsified itself).
   */
  public void recurse_definitions(NISuppressionSet ss) {

    // Get all of the new suppressions
    List<NISuppression> new_suppressions = new ArrayList<>();
    for (int i = 0; i < suppression_set.length; i++) {
      new_suppressions.addAll(suppression_set[i].recurse_definition(ss));
    }
    // This isn't necessarily true if the suppressee is of the same
    // class but doesn't match due to variable swapping.
    // assert new_suppressions.size() > 0;

    // Create a new suppression set with all of the suppressions.
    /*NNC:@MonotonicNonNull*/ NISuppression[] new_array =
        new NISuppression[suppression_set.length + new_suppressions.size()];
    for (int i = 0; i < suppression_set.length; i++) {
      new_array[i] = suppression_set[i];
    }
    for (int i = 0; i < new_suppressions.size(); i++) {
      new_array[suppression_set.length + i] = new_suppressions.get(i);
    }
    new_array = castNonNullDeep(new_array); // https://tinyurl.com/cfissue/986
    suppression_set = new_array;
  }

  /**
   * Swaps each suppressor and suppressee to the opposite variable order. Valid only on unary and
   * binary suppressors and suppressees.
   */
  public NISuppressionSet swap() {

    NISuppression[] swap_sups = new NISuppression[suppression_set.length];
    for (int i = 0; i < swap_sups.length; i++) {
      NISuppression std_sup = suppression_set[i];
      /*NNC:@MonotonicNonNull*/ NISuppressor[] sors = new NISuppressor[std_sup.suppressors.length];
      for (int j = 0; j < sors.length; j++) {
        sors[j] = std_sup.suppressors[j].swap();
      }
      sors = castNonNullDeep(sors); // https://tinyurl.com/cfissue/986
      swap_sups[i] = new NISuppression(sors, std_sup.suppressee.swap());
    }
    NISuppressionSet new_ss = new NISuppressionSet(swap_sups);
    return new_ss;
  }

  /** Returns the suppressee. */
  public NISuppressee get_suppressee() {
    return suppression_set[0].suppressee;
  }

  /** Clears the suppressor state in each suppression. */
  public void clear_state() {
    for (int i = 0; i < suppression_set.length; i++) {
      suppression_set[i].clear_state();
    }
  }

  /** Returns a string containing each suppression separated by commas. */
  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied NISuppressionSet this) {
    return "{ " + StringsPlume.join(", ", suppression_set) + " }";
  }
}
