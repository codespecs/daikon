package daikon;

import daikon.inv.*;

import java.util.*;

import utilMDE.*;

// This is a view on the full data (and maybe it does cacheing for a
// while).  This will be efficient for iteration (albeit with repetition),
// but inefficient for lookup (because we'd have to iterate over all
// entries -- or at least all keys -- to find any such instance, but we
// need to find them all to give a good result).

public abstract class PptSlice extends Ppt {

  public Ppt parent;
  public int arity;
  // var_infos appears in Ppt; don't repeat it here!!
  // public VarInfo[] var_infos;
  // cache of values from var_infos, to avoid repeated lookups
  public int[] value_indices;

  public boolean no_invariants = false;

  public Invariants invs;

  // This holds keys and elements of different types, depending on
  // he concrete child of PptSlice.
  HashMap values_cache;
  // These are used only when the values_cache has been set to null.
  int num_samples_post_cache = -2222;
  int num_mod_non_missing_samples_post_cache = -2222;
  int num_values_post_cache = -2222;
  String tuplemod_samples_summary_post_cache = "UNINITIALIZED";

  // true if we've seen all values and should not add the result of further
  // add() methods to values_cache.
  // This is rather a hack and should be removed later.
  public boolean already_seen_all = false;


  PptSlice(Ppt parent_, VarInfo[] var_infos_) {
    super();
    parent = parent_;
    var_infos = var_infos_;
    // System.out.println("in PptSlice(): this=" + this);
    // System.out.println("var_infos = " + var_infos + " = " + this.var_infos);
    // for (int i=0; i<var_infos.length; i++) {
    //   System.out.println("  var_infos[" + i + "] = " + var_infos[i]);
    // }
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i=0; i<var_infos.length-1; i++)
      Assert.assert(var_infos[i].varinfo_index < var_infos[i+1].varinfo_index);
    name = parent.name + varNames();
    arity = var_infos.length;
    value_indices = new int[arity];
    for (int i=0; i<arity; i++)
      value_indices[i] = var_infos[i].value_index;
    // values_cache = new HashMap();
    invs = new Invariants();
    // Don't do this; make someone else do it.
    // In particular, I want the subclass constructor to get called
    // before this is.
    // parent.addView(this);
  }

  public boolean usesVar(VarInfo vi) {
    return (ArraysMDE.indexOfEq(var_infos, vi) != -1);
  }

  // A reason to defer removal is because we're currently iterating over
  // the invariants by index rather than using an Iterator.
  private Vector invs_to_remove_deferred = null;
  // This to avoid constructing a new Vector every time through add().
  // One can just use this one (and be sure to clear it out afterward).
  private Vector itrd_cache = new Vector(1);

  // Avoid constructing a new Vector every time through this function.
  void defer_invariant_removal() {
    invs_to_remove_deferred = itrd_cache;
    Assert.assert(invs_to_remove_deferred.size() == 0);
  }

  void undefer_invariant_removal() {
    // I need to have invs_to_remove_deferred null, or
    // else removeInvariants just adds to that list!!
    // The old value is still available in itrd_cache.
    invs_to_remove_deferred = null;
    if (itrd_cache.size() > 0) {
      removeInvariants(itrd_cache);
      itrd_cache.clear();
    }
  }


  abstract public void addInvariant(Invariant inv);


  // I don't just use ppt.invs.remove because I want to be able to defer
  // and to take action if the vector becomes void.
  public void removeInvariant(Invariant inv) {
    Assert.assert(! no_invariants);
    if (Global.debugPptSlice)
      System.out.println("PptSlice.removeInvariant(" + inv.name() + ")" +
                         ((invs_to_remove_deferred != null)
                          ? " will be deferred"
                          : ""));
    Assert.assert(invs.contains(inv));
    if (invs_to_remove_deferred != null) {
      Assert.assert(! invs_to_remove_deferred.contains(inv));
      invs_to_remove_deferred.add(inv);
    } else {
      if (Global.debugInfer)
        System.out.println("PptSlice.removeInvariant(" + inv.name() + ")");
      boolean removed = invs.remove(inv);
      Assert.assert(removed);
      if (invs.size() == 0) {
        no_invariants = true;
        parent.removeView(this);
        // for good measure; shouldn't be necessary, but just in case there
        // is some other pointer to this.
        if (this instanceof PptSlice)
          ((PptSlice) this).clear_cache();
      }
    }
  }

  // I could make this more efficient, but it's probably fine as it is.
  public void removeInvariants(Vector to_remove) {
    // This must not call removeInvariant because that gets overridden.
    for (int i=0; i<to_remove.size(); i++) {
      removeInvariant((Invariant) to_remove.elementAt(i));
    }
  }

  void addView(PptSlice slice) {
    throw new Error("Don't add views on a slice.");
  }

  void addViews(Vector slices) {
    throw new Error("Don't add views on a slice.");
  }

  void removeView(Ppt slice) {
    throw new Error("Don't remove view from a slice.");
  }



  public void clear_cache() {
    // Don't do check_modbits()!  We might have only partially filled up
    // the cache.  Do this at call sites where appropriate.
    // Assert.assert(check_modbits());

    if (values_cache != null) {
      if (! no_invariants) {
        num_samples_post_cache = num_samples();
        num_mod_non_missing_samples_post_cache = num_mod_non_missing_samples();
        num_values_post_cache = num_values();
        tuplemod_samples_summary_post_cache = tuplemod_samples_summary();
      }
      values_cache = null;
    }
  }

  // These accessors are for abstract methods declared in Ppt
  abstract public int num_samples();
  abstract public int num_mod_non_missing_samples();
  abstract public int num_values();
  abstract public String tuplemod_samples_summary();

  boolean check_modbits () {
    Assert.assert(! no_invariants);
    if (num_mod_non_missing_samples() < num_values()) {
      String message = "Bad mod bits in dtrace file:\n"
        + "num_mod_non_missing_samples()=" + num_mod_non_missing_samples()
        + ", num_samples()=" + num_samples()
        + ", num_values()=" + num_values() + "\n"
        + "for " + name + "\n"
        + tuplemod_samples_summary() + "\n"
        + "Consider running modbit-munge.pl\n"
        + ((values_cache == null)
           ? "Values cache has been cleared\n"
           : "Values cache has not been cleared");
      if (! Daikon.disable_modbit_check_message) {
        System.out.println(message);
        if (values_cache != null) {
          System.out.println("To do:  Dump values_cache");
          // This is probably specific to the specializers of PptSlice.
          // values_cache.dump();
        }
      }
      if (! Daikon.disable_modbit_check_error)
        throw new Error(message);
    }
    return true;
  }

  abstract void instantiate_invariants(int pass);

}

/// These functions create real new Ppt objects as oppose to slices.

// def dict_of_tuples_slice_2(dot, i1, i2):
//     """Input: a dictionary mapping a tuple of elements to a count, and a
//     list of indices.
//     Output: a dictionary mapping a subset of the original elements to a count.
//     The subset is chosen according to the input indices."""
//
//     result = {}
//     for (key_tuple, count) in dot.items():
//         # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
//         sliced_tuple = (key_tuple[i1][0], key_tuple[i2][0])
//         modified = key_tuple[i1][1] and key_tuple[i2][1]
//         this_counts = result.get(sliced_tuple, [0, 0])
//         result[sliced_tuple] = this_counts
//         this_counts[0] = this_counts[0] + count
//         if modified:
//             this_counts[1] = this_counts[1] + count
//     return result
//
// def dict_of_tuples_slice_3(dot, i1, i2, i3):
//     """Input: a dictionary mapping a tuple of elements to a count, and a
//     list of indices.
//     Output: a dictionary mapping a subset of the original elements to a count.
//     The subset is chosen according to the input indices."""
//
//     result = {}
//     for (key_tuple, count) in dot.items():
//         # sliced_tuple = util.slice_by_sequence(key_tuple, indices)
//         sliced_tuple = (key_tuple[i1][0], key_tuple[i2][0], key_tuple[i3][0])
//         modified = key_tuple[i1][1] and key_tuple[i2][1] and key_tuple[i3][1]
//         this_counts = result.get(sliced_tuple, [0, 0])
//         result[sliced_tuple] = this_counts
//         this_counts[0] = this_counts[0] + count
//         if modified:
//             this_counts[1] = this_counts[1] + count
//     return result
