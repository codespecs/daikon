package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.suppress.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.*;
import java.util.*;

/**
 * Class that defines a set of non-instantiating suppressions for a single
 * invariant (suppressee).  Basically this just a list of suppressions, but
 * creating a class allows this to be somewhat more clear.
 */
public class NISuppressionSet {

  public static final Logger debug
        = Logger.getLogger ("daikon.suppress.NISuppressionSet");

  NISuppression[] suppression_set;

  public NISuppressionSet (NISuppression[] suppressions) {

    suppression_set = suppressions;
  }


  public Iterator iterator() {
    return (Arrays.asList(suppression_set).iterator());
  }

  /**
   * Adds this set to the suppressor map.  The map is from the class of
   * the suppressor to this. If the same suppressor class appears more
   * than once, the suppression is only added once.
   */
  public void add_to_suppressor_map (Map suppressor_map) {

    Set all_suppressors = new LinkedHashSet();

    // Loop through each suppression in the suppression set
    for (int i = 0; i < suppression_set.length; i++) {
      NISuppression suppression = suppression_set[i];

      // Loop through each suppressor in the suppression
      for (Iterator j = suppression.suppressor_iterator(); j.hasNext(); ) {
        NISuppressor suppressor = (NISuppressor) j.next();

        // If we have seen this suppressor already, skip it
        if (all_suppressors.contains (suppressor.get_inv_class()))
          continue;

        // Note that we have now seen this suppressor invariant class
        all_suppressors.add (suppressor.get_inv_class());


        // Get the list of suppression sets for this suppressor.  Create it
        // if this is the first one.  Add this set to the list
        List suppression_set_list
                     = (List) suppressor_map.get (suppressor.get_inv_class());
        if (suppression_set_list == null) {
          suppression_set_list = new ArrayList();
          suppressor_map.put (suppressor.get_inv_class(),
                              suppression_set_list);
        }
        suppression_set_list.add (this);
      }
    }
  }

  public void falsified (Invariant inv, List new_invs) {

    // Get the ppt we are working in
    PptTopLevel ppt = inv.ppt.parent;

    // For now all suppressors are unary/binary and all suppressees are ternary
    Assert.assertTrue (inv.ppt.var_infos.length < 3);
    Assert.assertTrue (suppression_set[0].suppressee.var_count == 3);

    // Create all of the valid ternary slices that use the vars from inv
    // and check to see if the invariant should be created for each slice
    if (inv.ppt.var_infos.length == 2) {
      VarInfo[] vis = new VarInfo[3];
      VarInfo v1 = inv.ppt.var_infos[0];
      VarInfo v2 = inv.ppt.var_infos[1];
      VarInfo[] leaders = ppt.equality_view.get_leaders_sorted();
      for (int i = 0; i < leaders.length; i++) {
        VarInfo l = leaders[i];
        if (!ppt.is_slice_ok (l, v1, v2))
          continue;
        if (l.missingOutOfBounds() || v1.missingOutOfBounds()
            || v2.missingOutOfBounds())
          continue;

        // Order the variables,
        if (l.varinfo_index <= v1.varinfo_index) {
          vis[0] = l;
          vis[1] = v1;
          vis[2] = v2;
        } else if (l.varinfo_index <= v2.varinfo_index) {
          vis[0] = v1;
          vis[1] = l;
          vis[2] = v2;
        } else {
          vis[0] = v1;
          vis[1] = v2;
          vis[2] = l;
        }

        if (NIS.debug.isLoggable (Level.FINE))
          NIS.debug.fine ("processing slice " + Debug.toString(vis)
                       + " in ppt " + ppt.name() + " with " + ppt.numViews());

        check_falsified (ppt, vis, inv, new_invs);
      }
    } else /* must be unary */ {
      VarInfo[] vis = new VarInfo[3];
      VarInfo v1 = inv.ppt.var_infos[0];
      VarInfo[] leaders = ppt.equality_view.get_leaders_sorted();
      for (int i = 0; i < leaders.length; i++) {
        VarInfo l1 = leaders[i];
        for (int j = i; j < leaders.length; j++) {
          VarInfo l2 = leaders[j];

          // Make sure the slice is interesting
          if (v1.missingOutOfBounds() || l1.missingOutOfBounds()
              || l2.missingOutOfBounds())
            continue;
          if (!ppt.is_slice_ok (v1, l1, l2))
            continue;

          // Sort the variables
          if (v1.varinfo_index <= l1.varinfo_index) {
            vis[0] = v1;
            vis[1] = l1;
            vis[2] = l2;
          } else if (v1.varinfo_index <= l2.varinfo_index) {
            vis[0] = l1;
            vis[1] = v1;
            vis[2] = l2;
          } else {
            vis[0] = l1;
            vis[1] = l2;
            vis[2] = v1;
          }


          if (NIS.debug.isLoggable (Level.FINE))
            NIS.debug.fine ("processing slice " + Debug.toString(vis)
                + " in ppt " + ppt.name() + " with " + ppt.numViews());

          check_falsified (ppt, vis, inv, new_invs);
        }
      }
    }
  }

  /**
   * Checks the falsified invariant against the slice specified by vis.
   * If the falsification of inv removed the last valid suppression then
   * instantiates the suppressee
   */
  public void check_falsified (PptTopLevel ppt, VarInfo[] vis, Invariant inv,
                               List new_invs) {

    // process each suppression in the set, marking each suppressor as
    // to whether it is true, false, or matches the falsified inv
    // If any particular suppression is still valid, just return as there
    // is nothing to be done (the suppressee is still suppressed)
    for (int i = 0; i < suppression_set.length; i++ ) {
      String status = suppression_set[i].check (ppt, vis, inv);
      if (status == NIS.VALID) {
        if (NIS.debug.isLoggable (Level.FINE))
          NIS.debug.fine ("suppression " + suppression_set[i] + " is valid");
        return;
      }
      Assert.assertTrue (status != NIS.MISSING);
    }

    if (NIS.debug.isLoggable (Level.FINE))
      NIS.debug.fine ("After check, suppression set: " + this);

    // There are no remaining valid (true) suppressions.  If inv is the
    // first suppressor to be removed from any suppressions, then this
    // falsification removed the last valid suppression.  In that case we
    // need to instantiate the suppressee.
    for (int i = 0; i < suppression_set.length; i++) {
      if (suppression_set[i].invalidated()) {
        instantiate (inv.ppt.parent, vis, new_invs);
        return;
      }
    }
  }

  /**
   * Determines whether or not the suppression set is valid in the
   * specified slice.  The suppression set is valid if any of its
   * suppressions are valid.  A suppression is valid if all of its
   * non-missing suppressors are true.
   */
  public boolean suppressed (PptSlice slice) {

    return (suppressed (slice.parent, slice.var_infos));
  }

  /**
   * Determines whether or not the suppression set is valid in the
   * specified ppt and var_infos.  The suppression set is valid if any
   * of its suppressions are valid.  A suppression is valid if all of
   * its non-missing suppressors are true.
   */
  public boolean suppressed (PptTopLevel ppt, VarInfo[] var_infos) {

    // Check each suppression to see if it is valid
    for (int i = 0; i < suppression_set.length; i++ ) {
      String status = suppression_set[i].check (ppt, var_infos, null);
      if ((status == NIS.VALID) || (status == NIS.MISSING)) {
        if (Debug.logOn() || NIS.debug.isLoggable (Level.FINE))
          Debug.log (NIS.debug, getClass(), ppt, var_infos, "suppression "
            + suppression_set[i] + " is " + status + " in ppt " + ppt
            + " with var infos " + VarInfo.toString (var_infos));
        return (true);
      }
    }

    if (Debug.logOn() || NIS.debug.isLoggable (Level.FINE))
      Debug.log (NIS.debug, getClass(), ppt, var_infos, "suppression " + this
                  + " is not valid in ppt " + ppt + " with var infos "
                  + VarInfo.toString (var_infos));
    return (false);
  }

  /**
   * Instantiates the suppressee over the specified variables in the
   * specified ppt.  The invariant is added to the new_invs list, but
   * not to the slice.  The invariant is added to the slice later when
   * the sample is applied to it.  That guarantees that it is only applied
   * the sample once.
   */
  public void instantiate (PptTopLevel ppt, VarInfo[] vis, List new_invs) {

    if (Assert.enabled) {
      for (int i = 0; i < vis.length; i++)
        Assert.assertTrue (!vis[i].missingOutOfBounds());
    }

    // Find the slice and create it if it is not already there.
    // Note that we must make a copy of vis.  vis is used to create each
    // slice and will change after we create the slice which leads to
    // very interesting results.
    PptSlice slice = ppt.findSlice (vis);
    if (slice == null) {
      VarInfo[] newvis = (VarInfo[]) vis.clone();
      slice = new PptSlice3 (ppt, newvis);
      // Fmt.pf ("Adding slice " + slice);
      ppt.addSlice (slice);
    }

    // Create the new invariant
    NISuppressee suppressee = suppression_set[0].suppressee;
    Invariant inv = suppressee.instantiate (slice);

    if (Debug.logOn() || NIS.debug.isLoggable (Level.FINE))
      inv.log (NIS.debug, "Adding " + inv.format()
               + " from nis suppression set " + this);

    // Make sure the invariant isn't already in the new_invs list
    if (Daikon.dkconfig_internal_check) {
      for (Iterator i = new_invs.iterator(); i.hasNext(); ) {
        Invariant new_inv = (Invariant) i.next();
        if ((new_inv.getClass() == inv.getClass()) && (new_inv.ppt == slice))
          Assert.assertTrue (false, Fmt.spf ("inv %s:%s already in new_invs "
                        + "(slice %s)", inv.getClass(), inv.format(), slice));
      }
    }

    // If the invariant is in the global slice, don't create it (since
    // invariants true at the global ppt are known to be true here as well)
    PptSlice gslice = PptSlice.find_global_slice (vis);
    if ((gslice != null) && gslice.contains_inv_exact(inv) &&inv.isFlowable()){
      if (Debug.logOn() || NIS.debug.isLoggable (Level.FINE))
        inv.log (NIS.debug, "Ignoring  " + inv.format()
                 + " - it is in the global ppt");
        return;
    }

    // Add the invariant to the new invariant list
    new_invs.add (inv);

    if (Daikon.dkconfig_internal_check) {
      if (slice.contains_inv_exact (inv)) {
        // Print all unary and binary invariants over the same variables
        for (int i = 0; i < vis.length; i++) {
          PrintInvariants.print_all_invs (ppt, vis[i], "  ");
        }
        PrintInvariants.print_all_invs (ppt, vis[0], vis[1], "  ");
        PrintInvariants.print_all_invs (ppt, vis[1], vis[2], "  ");
        PrintInvariants.print_all_invs (ppt, vis[0], vis[2], "  ");
        Debug.check (Daikon.all_ppts, "assert failure");
        Assert.assertTrue (false, Fmt.spf ("inv %s:%s already in slice %s",
                        inv.getClass(), inv.format(), slice));
      }
    }

  }


  /**
   * clears the suppressor state in each suppression
   */
  public void clear_state () {
    for (int i = 0; i < suppression_set.length; i++ ) {
      suppression_set[i].clear_state();
    }
  }

  /**
   * Returns a string containing each suppression separated by commas
   */
  public String toString() {
    return UtilMDE.join(suppression_set, ", ");
  }

}
