package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.ternary.*;
import daikon.suppress.*;
import utilMDE.*;

import java.lang.reflect.*;
import java.util.logging.Logger;
import java.util.*;

/**
 * Defines a suppressee for non-instantiating suppression.  A suppressee
 * consists only of the class at this point since ternary invariants
 * only require the class to define them fully (permutations are built
 * into the class name).  When binary invariants are suppressed additional
 * information will need to be included.
 */
public class NISuppressee {

  Class sup_class;
  int var_count;
  Invariant sample_inv;

  public NISuppressee (Class cls, int var_count) {
    sup_class = cls;
    Assert.assertTrue ((var_count >= 1) && (var_count <=3));
    this.var_count = var_count;

    try {
      Method instantiate = cls.getMethod ("instantiate",
                                                new Class[] {PptSlice.class});
      sample_inv = (Invariant)instantiate.invoke (null, new Object[] {null});
    } catch (Exception e) {
      throw new RuntimeException ("error instantiating invariant "
                                  + cls.getName() + ": " + e);
    }
  }

  /**
   * Instantiates the suppressee invariant on the specified slice.
   */
  public Invariant instantiate (PptSlice slice) {

    Invariant inv = (Invariant) sample_inv.clone();
    inv.ppt = slice;
    if (Debug.logOn())
      inv.log ("Created " + inv.format());
    return (inv);
  }

  public InvariantStatus check (ValueTuple vt, VarInfo[] vis) {

    VarInfo v1 = vis[0];
    VarInfo v2 = vis[1];
    VarInfo v3 = vis[2];

    if (v1.isMissing(vt) || v2.isMissing(vt) || v3.isMissing(vt))
      return InvariantStatus.NO_CHANGE;

    // Fmt.pf ("%s [%s], %s [%s], %s [%s]", v1.name.name(), v1.file_rep_type,
    //         v2.name.name(), v2.file_rep_type, v3.name.name(),
    //         v3.file_rep_type);
    // Fmt.pf ("suppressee " + this);
    TernaryInvariant ternary_inv = (TernaryInvariant) sample_inv;
    return ternary_inv.check (vt.getValue(v1), vt.getValue(v2),
                                       vt.getValue(v3), 1, 1);
  }
  /**
   * Instantiates the suppressee invariant on the slice specified
   * by vis in the specified ppt.  If the slice is not currently there,
   * it will be created.
   */
  public Invariant instantiate (VarInfo[] vis, PptTopLevel ppt) {

    PptSlice slice = ppt.get_or_instantiate_slice (vis);
    return (instantiate (slice));
  }

  /**
   * Instantiates the suppressee invariant on all of the slices
   * specified by vis in the specified ppt.  Multiple slices can
   * be specified by vis if a slot in vis is null.  The slot will be
   * filled by all leaders that can correctly fill the slot and an
   * invariant created for each. @return a list of all of the created
   * invariants.
   */
  public List/*Invariant*/ instantiate_all (VarInfo[] vis, PptTopLevel ppt) {

    List/*Invariant*/ created_list = new ArrayList();

    // Check for empty slots in vis, fail if there is more than one
    int missing_index = -1;
    for (int i = 0; i < vis.length; i++)
      if (vis[i] == null) {
        Assert.assertTrue (missing_index == -1, "Multiple empty vars");
        missing_index = i;
      }

    // If all of the slots were full, create the invariant
    if (missing_index == -1) {
      if (ppt.is_slice_ok (vis, vis.length))
        created_list.add (instantiate (vis, ppt));
      return (created_list);
    }

    // Fill in the missing slot with each possible matching leader and
    // create an invariant for it.
    VarInfo leaders[] = ppt.equality_view.get_leaders_sorted();
    for (int i = 0; i < leaders.length; i++) {
      VarInfo v = leaders[i];
      vis[missing_index] = v;
      if (!ppt.vis_order_ok (vis))
        continue;
      if (!ppt.is_slice_ok (vis, vis.length))
        continue;
      created_list.add (instantiate (vis, ppt));
    }

    return (created_list);
  }

  /**
   * Finds the suppressee invariants on all of the slices
   * specified by vis in the specified ppt.  Multiple slices can
   * be specified by vis if a slot in vis is null.  The slot will be
   * filled by all leaders that can correctly fill the slot and SupInv
   * created for each. @return a list describing all of the invariants
   */
  public List/*Invariant*/ find_all (VarInfo[] vis, PptTopLevel ppt) {

    List/*Invariant*/ created_list = new ArrayList();

    // Check for empty slots in vis, fail if there is more than one
    int missing_index = -1;
    for (int i = 0; i < vis.length; i++)
      if (vis[i] == null) {
        Assert.assertTrue (missing_index == -1, "Multiple empty vars");
        missing_index = i;
      }

    // If all of the slots were full, specify the invariant
    if (missing_index == -1) {
      if (ppt.is_slice_ok (vis, vis.length))
        created_list.add (new NIS.SupInv (this, vis));
      return (created_list);
    }

    // Fill in the missing slot with each possible matching leader and
    // create an invariant for it.
    VarInfo leaders[] = ppt.equality_view.get_leaders_sorted();
    for (int i = 0; i < leaders.length; i++) {
      VarInfo v = leaders[i];
      vis[missing_index] = v;
      if (!ppt.vis_order_ok (vis))
        continue;
      if (!ppt.is_slice_ok (vis, vis.length))
        continue;
      // Fmt.pf ("find_all: %s [%s]", v.name.name(), v.rep_type);
      created_list.add (new NIS.SupInv (this, (VarInfo[]) vis.clone()));
    }

    return (created_list);
  }

  public String toString() {

    return (UtilMDE.unqualified_name (sup_class));
  }

}
