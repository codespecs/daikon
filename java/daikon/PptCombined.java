package daikon;

import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import daikon.FileIO.ParentRelation;

import java.util.*;

/**
 * A program point which consists of a number of program points.  Invariants
 * are looked for over all combinations of variables from all of the program
 * points that make up the combined ppt.
 */
public class PptCombined extends PptTopLevel {

  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20071129L;

  /** List of ppts that make up this combined ppt **/
  List<PptTopLevel> ppts;

  private boolean debug = true;

  public PptCombined (List<PptTopLevel> ppts) {

    super ("combined_" + ppts.get(0).name(), PptType.COMBINED_BASIC_BLOCK,
           new ArrayList<ParentRelation>(), EnumSet.noneOf (PptFlags.class),
           null, ppts.get(0).function_id, combined_vis (ppts));
    this.ppts = ppts;
    init();
    System.out.printf ("Variables for combined ppt %s%n", name());
    if (debug) {
      for (VarInfo vi : var_infos) {
        System.out.printf ("  %s [%d/%d]%n", vi.name(), vi.varinfo_index,
                           vi.value_index);
      }
    }
  }

  /** Returns a name basic on its constituent ppts **/
  public String name() {
    String name = ppts.get(0).name();
    name += ".." + ppts.get(ppts.size()-1).ppt_name.name();
    return name;
  }

  /**
   * Initialize the ppt.  This is similar to init_ppt in Daikon.java
   * except that orig variables are never created (they don't make sense
   * in this context).  Splitters are also not created.  Equality ses are
   * always setup (since this is always a leaf in the hierarchy)
   **/
  private void init() {
    if (!Derivation.dkconfig_disable_derived_variables) {
      create_derived_variables();
    }

    if (!Daikon.using_DaikonSimple && Daikon.use_equality_optimization) {
      equality_view = new PptSliceEquality(this);
      equality_view.instantiate_invariants();
    }
  }

  /**
   * Add the current sample.  The last samples for each of the program
   * points that make up the combined program point must have been added
   * to their last_values field
   */
  public void add_combined() {

    // Number of values in the combined ValueTuple
    int vals_array_size = var_infos.length - num_static_constant_vars;

    // Allocate arrays for the combined values and mod information
    Object[] vals = new Object[vals_array_size];
    int[] mods = new int[vals_array_size];
    ValueTuple partial_vt = ValueTuple.makeUninterned (vals, mods);

    // Copy the values from each constituent program point
    int index = 0;
    for (PptTopLevel ppt : ppts) {
      int filled_slots = ppt.num_orig_vars + ppt.num_tracevars;
      for (int i = 0; i < filled_slots; i++) {
        if (ppt.last_values == null) {
          //System.out.printf ("no last valfor %s in %s%n", ppt.name(), name());
          vals[index] = null;
          mods[index] = ValueTuple.MISSING_NONSENSICAL;
        } else {
          vals[index] = ppt.last_values.vals[i];
          assert (vals[index] != null);
          mods[index] = ppt.last_values.mods[i];
        }
        assert (!ppt.var_infos[i].isDerived());
        index++;
      }
    }
    assert (index == (num_orig_vars + num_tracevars));

    // add the derived variables
    while (index < vals_array_size) {
      assert (var_infos[index].isDerived());
      ValueAndModified vm =
        var_infos[index].derived.computeValueAndModified(partial_vt);
      vals[index] = vm.value;
      mods[index]= vm.modified;
      index++;
    }

    // Create an interned ValueTuple
    ValueTuple vt = new ValueTuple (vals, mods);

    // Add the sample
    add_bottom_up (vt, 1);

  }

  /**
   * Build a combined VarInfo array for all of the ppts
   */
  private static VarInfo[] combined_vis (List<PptTopLevel> ppts) {

    assert (ppts.size() > 0) : "No ppts in list";

    // Allocate an array of the combined size
    int len = 0;
    for (PptTopLevel ppt : ppts) {
      len += ppt.var_infos.length;
    }
    VarInfo[] vis = new VarInfo[len];

    // Create a new VarInfo for each VarInfo in the ppt list
    int index = 0;
    for (PptTopLevel ppt : ppts) {
      for (VarInfo vi : ppt.var_infos) {
        if (vi.isDerived())
          continue;
        vis[index++] = new VarInfo (vi.vardef);
      }
    }

    return (vis);
  }


  /**
   * Creates combined program points that cover multiple basic
   * blocks.  Each basic block ppt is combined with any basic blocks
   * that dominate it (always occur before it).
   *
   * The input is a list of the basic block ppts that make up the
   * function.  Each bb ppt contains a list of the names of all of the
   * basic blocks that directly succeed it.  That list is used to
   * calculate the dominators.
   *
   * Each program point in the function is modified as follows: <ul>
   *   <li> Its combined_ppts_init flag is set to true
   *   <li> Its combined_ppt field is set to point to the combined
   *    program point that should be processed when this bb ppt is
   *    executed.   This field may be null if this bb ppt is completely
   *    subsumed by other combined ppts
   *    <li> Its combined_subsumed boolean field is set to true if this
   *    ppt is subsumed by a combine dprogram point, false otherwise.
   *
   * The current implementation is just an example that creates a combined
   * program point for each program point with exactly one successor
   */
  public static void combine_func_ppts (PptMap all_ppts,
                                        List<PptTopLevel> func_ppts) {

    // Loop through each basic block ppt in the function
    for (PptTopLevel ppt : func_ppts) {

      // Mark this ppt as initialized
      ppt.combined_ppts_init = true;

      // Skip any ppt that doesn't have exactly one successor
      if ((ppt.ppt_successors == null) || (ppt.ppt_successors.size() != 1))
        continue;

      // Get the successor ppt
      String succ = ppt.ppt_successors.get(0);
      PptTopLevel succ_ppt = all_ppts.get (succ);

      // If the successor already has a combined ppt, skip it
      if (succ_ppt.combined_ppt != null)
        continue;

      // Build the combined program point that includes this ppt and its
      // successor
      List<PptTopLevel> combined_ppts = new ArrayList<PptTopLevel>();
      combined_ppts.add (ppt);
      combined_ppts.add (succ_ppt);
      succ_ppt.combined_ppt = new PptCombined (combined_ppts);

      // Presume that the new combined ppt subsumes both of these ppts
      ppt.combined_subsumed = true;
      succ_ppt.combined_subsumed = true;
    }
  }

}
