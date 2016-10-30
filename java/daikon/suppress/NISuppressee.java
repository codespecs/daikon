package daikon.suppress;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.*;
import daikon.inv.ternary.*;
import daikon.inv.unary.*;
import java.lang.reflect.*;
import java.util.*;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
import typequals.*;
*/

/**
 * Defines a suppressee for non-instantiating suppression. A suppressee consists only of the class
 * at this point since ternary invariants only require the class to define them fully (permutations
 * are built into the class name). When binary invariants are suppressed additional information will
 * need to be included.
 */
public class NISuppressee {

  public Class<? extends Invariant> sup_class;
  public int var_count;
  public /*@Prototype*/ Invariant sample_inv;

  public NISuppressee(Class<? extends Invariant> cls, int var_count) {
    sup_class = cls;
    assert (var_count >= 1) && (var_count <= 3);
    this.var_count = var_count;

    try {
      Method get_proto = cls.getMethod("get_proto", new Class<?>[] {});
      @SuppressWarnings({"nullness", "prototype"}) // reflective invocation is nullness-correct
      /*@NonNull*/ /*@Prototype*/ Invariant sample_inv_local =
          (/*@Prototype*/ Invariant) get_proto.invoke(null, new Object[] {});
      sample_inv = sample_inv_local;
      assert sample_inv != null : cls.getName();
    } catch (Exception e) {
      throw new RuntimeException("error instantiating invariant " + cls.getName() + ": " + e);
    }
  }

  /** Define a binary suppressee on the specified class with the specified variable order. */
  public NISuppressee(Class<? extends Invariant> cls, boolean swap) {
    sup_class = cls;
    this.var_count = 2;

    try {
      Method get_proto = cls.getMethod("get_proto", new Class<?>[] {boolean.class});
      @SuppressWarnings({"nullness", "prototype"}) // reflective invocation is nullness-correct
      /*@NonNull*/ /*@Prototype*/ Invariant sample_inv_local =
          (/*@Prototype*/ Invariant) get_proto.invoke(null, new Object[] {Boolean.valueOf(swap)});
      sample_inv = sample_inv_local;
      assert sample_inv != null : cls.getName();
    } catch (Exception e) {
      throw new RuntimeException(
          "error instantiating binary invariant " + cls.getName() + ": " + e);
    }
  }

  /** Instantiates the suppressee invariant on the specified slice. */
  public /*@Nullable*/ Invariant instantiate(PptSlice slice) {

    Invariant inv = sample_inv.instantiate(slice);
    if (Debug.logOn()) {
      if (inv != null) {
        inv.log("Created %s", inv.format());
      } else {
        Debug.log(sup_class, slice, "Didn't create, instantiate returned null");
      }
    }
    return inv;
  }

  /** Checks this invariant against the specified sample and returns the status. */
  public InvariantStatus check(ValueTuple vt, VarInfo[] vis) {

    // Nothing to check if any variable is missing
    for (int i = 0; i < vis.length; i++) {
      if (vis[i].isMissing(vt)) {
        return InvariantStatus.NO_CHANGE;
      }
    }

    if (var_count == 3) {
      TernaryInvariant ternary_inv = (TernaryInvariant) sample_inv;
      return ternary_inv.check(vt.getValue(vis[0]), vt.getValue(vis[1]), vt.getValue(vis[2]), 1, 1);
    } else if (var_count == 2) {
      if (!(sample_inv instanceof BinaryInvariant)) {
        throw new Error("not binary: " + sample_inv.getClass());
      }
      BinaryInvariant binary_inv = (BinaryInvariant) sample_inv;
      // System.out.printf ("checking %s over %s=%s and %s=%s%n", sample_inv.getClass(),
      //        vis[0].name(), vt.getValue(vis[0]),
      //        vis[1].name(), vt.getValue(vis[1]));
      return binary_inv.check_unordered(vt.getValue(vis[0]), vt.getValue(vis[1]), 1, 1);
    } else /* must be unary */ {
      UnaryInvariant unary_inv = (UnaryInvariant) sample_inv;
      return unary_inv.check(vt.getValue(vis[0]), 1, 1);
    }
  }

  /**
   * Instantiates the suppressee invariant on the slice specified by vis in the specified ppt. If
   * the slice is not currently there, it will be created.
   */
  public /*@Nullable*/ Invariant instantiate(VarInfo[] vis, PptTopLevel ppt) {

    PptSlice slice = ppt.get_or_instantiate_slice(vis);
    return (instantiate(slice));
  }

  //   /**
  //    * Instantiates the suppressee invariant on all of the slices
  //    * specified by vis in the specified ppt.  Multiple slices can
  //    * be specified by vis if a slot in vis is null.  The slot will be
  //    * filled by all leaders that can correctly fill the slot and an
  //    * invariant created for each. @return a list of all of the created
  //    * invariants.
  //    */
  //   public List<Invariant> instantiate_all (VarInfo[] vis, PptTopLevel ppt) {
  //
  //     List<Invariant> created_list = new ArrayList<Invariant>();
  //
  //     // Check for empty slots in vis, fail if there is more than one
  //     int missing_index = -1;
  //     for (int i = 0; i < vis.length; i++)
  //       if (vis[i] == null) {
  //         assert missing_index == -1 : "Multiple empty vars";
  //         missing_index = i;
  //       }
  //
  //     // If all of the slots were full, create the invariant
  //     if (missing_index == -1) {
  //       if (ppt.is_slice_ok (vis, vis.length)) {
  //         Invariant inv = instantiate (vis, ppt);
  //         if (inv != null)
  //           created_list.add (inv);
  //       }
  //       return created_list;
  //     }
  //
  //     // Fill in the missing slot with each possible matching leader and
  //     // create an invariant for it.
  //     VarInfo leaders[] = ppt.equality_view.get_leaders_sorted();
  //     for (int i = 0; i < leaders.length; i++) {
  //       VarInfo v = leaders[i];
  //       vis[missing_index] = v;
  //       if (!ppt.vis_order_ok (vis))
  //         continue;
  //       if (!ppt.is_slice_ok (vis, vis.length))
  //         continue;
  //       Invariant inv = instantiate (vis, ppt);
  //       if (inv != null)
  //         created_list.add (inv);
  //     }
  //
  //     return created_list;
  //   }

  /**
   * Finds the suppressee invariants on all of the slices specified by vis in the specified ppt.
   * Multiple slices can be specified by vis if a slot in vis is null. The slot will be filled by
   * all leaders that can correctly fill the slot and SupInv created for each.
   *
   * @param cinvs an array of the actual invariants that were found for each slot. It is used for
   *     for debug printing only.
   * @return a list describing all of the invariants
   */
  /*@RequiresNonNull("#2.equality_view")*/
  public List<NIS.SupInv> find_all(
      VarInfo[] vis, PptTopLevel ppt, /*@Nullable*/ Invariant /*@Nullable*/ [] cinvs) {

    List<NIS.SupInv> created_list = new ArrayList<NIS.SupInv>();

    // Check for empty slots in vis, fail if there is more than one
    int missing_index = -1;
    for (int i = 0; i < vis.length; i++)
      if (vis[i] == null) {
        assert missing_index == -1 : "Multiple empty vars";
        missing_index = i;
      } else {
        assert !vis[i].missingOutOfBounds();
      }

    // If all of the slots were full, specify the invariant
    if (missing_index == -1) {
      if (ppt.is_slice_ok(vis, vis.length)
          && NISuppression.vis_compatible(vis)
          && sample_inv.valid_types(vis)) {
        NIS.SupInv sinv = new NIS.SupInv(this, vis, ppt);
        sinv.log("Created for invariants: " + Arrays.toString(cinvs));
        created_list.add(sinv);
      }
      return created_list;
    }

    // Fill in the missing slot with each possible matching leader and
    // create an invariant for it.
    VarInfo leaders[] = ppt.equality_view.get_leaders_sorted();
    for (int i = 0; i < leaders.length; i++) {
      VarInfo v = leaders[i];
      vis[missing_index] = v;
      if (v.missingOutOfBounds()) continue;
      if (!ppt.vis_order_ok(vis)) continue;
      if (!ppt.is_slice_ok(vis, vis.length)) continue;
      if (!NISuppression.vis_compatible(vis)) continue;
      if (!sample_inv.valid_types(vis)) continue;
      NIS.SupInv sinv = new NIS.SupInv(this, vis.clone(), ppt);
      sinv.log("Unspecified variable = " + v.name());
      sinv.log("Created for invariants: " + Arrays.toString(cinvs));
      created_list.add(sinv);
    }
    return created_list;
  }

  /**
   * Returns the swap variable setting for the suppressee. Returns false if the suppressee is not a
   * binary invariant, is symmetric, or permutes by changing classes.
   */
  public boolean get_swap() {

    if (var_count != 2) return false;

    BinaryInvariant binv = (BinaryInvariant) sample_inv;
    if (binv.is_symmetric()) return false;
    return (binv.get_swap());
  }

  /**
   * Returns a new suppressee that is the same as this one except that its variables are swapped.
   */
  public NISuppressee swap() {
    assert var_count == 2;
    BinaryInvariant binv = (BinaryInvariant) sample_inv;
    if (binv != null) assert !binv.is_symmetric();
    if ((binv == null) || binv.get_swap()) {
      return (new NISuppressee(sup_class, false));
    } else {
      return (new NISuppressee(sup_class, true));
    }
  }

  /*@SideEffectFree*/
  public String toString(/*>>>@GuardSatisfied NISuppressee this*/) {

    String extra = "";
    if (var_count == 2) {
      BinaryInvariant binv = (BinaryInvariant) sample_inv;
      if (binv == null) {
        extra = " [null sample inv]";
      } else if (binv.is_symmetric()) {
        extra = " [sym]";
      } else if (binv.get_swap()) {
        extra = " [swap]";
      }
    }
    return sup_class.getSimpleName() + extra;
  }
}
