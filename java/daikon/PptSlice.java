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
  public static final String lineSep = Global.lineSep;

  public boolean debugged;

  public Ppt parent;
  public int arity;
  // var_infos appears in Ppt; don't repeat it here!!
  // public VarInfo[] var_infos;
  // Cache of values from var_infos, to avoid repeated lookups.
  // value_indices[i] == var_infos[i].value_index, but looking up in
  // this array should be cheaper than looking up in var_infos.
  public int[] value_indices;

  // If true, then we are in the process of deleting this invariant.
  // It should only be on a list of deferred to-be-deleted invariants.
  // Thus, we should never see this non-false.
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

  // This is rather a hack and should be removed later.

  // True if we've seen all values and are performing add() based on values
  // already in the values_cache; so add() should not add its arguments to
  // values_cache.
  public boolean already_seen_all = false;


  PptSlice(Ppt parent, VarInfo[] var_infos) {
    super(parent.name + varNames(var_infos));
    this.parent = parent;
    this.var_infos = var_infos;
    // System.out.println("in PptSlice(): this=" + this);
    // System.out.println("var_infos = " + var_infos + " = " + this.var_infos);
    // for (int i=0; i<var_infos.length; i++) {
    //   System.out.println("  var_infos[" + i + "] = " + var_infos[i]);
    // }
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i=0; i<var_infos.length-1; i++)
      Assert.assert(var_infos[i].varinfo_index < var_infos[i+1].varinfo_index);
    arity = var_infos.length;
    value_indices = new int[arity];
    for (int i=0; i<arity; i++)
      value_indices[i] = var_infos[i].value_index;
    // This is now done in the child classes (PptSlice1, etc.)
    // values_cache = new HashMap();
    invs = new Invariants();
    // Don't do this; make someone else do it.
    // In particular, I want the subclass constructor to get called
    // before this is.
    // parent.addView(this);

    // This comes after setting all other variables, as the function call may use name, arity, var_infos, etc.
    debugged = (Global.debugPptSliceSpecific
                && Global.isDebuggedPptSlice(this));
  }

  public boolean usesVar(VarInfo vi) {
    return (ArraysMDE.indexOfEq(var_infos, vi) != -1);
  }

  public boolean usesVar(String name) {
    Assert.assert(Intern.isInterned(name));
    for (int i=0; i<var_infos.length; i++) {
      if (var_infos[i].name == name) {
        return true;
      }
    }
    return false;
  }


  // When non-null, removeInvariants adds to this list instead of actually
  // removing.  (We might need to defer removal because we're currently
  // iterating over the invariants by index rather than using an Iterator.)
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


  public abstract void addInvariant(Invariant inv);


  // I don't just use ppt.invs.remove because I want to be able to defer
  // and to take action if the vector becomes void.
  public void removeInvariant(Invariant inv) {
    Assert.assert(! no_invariants);
    if (this.debugged || Global.debugPptSlice)
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
      // This increment could also have been in Invariant.destroy().
      Global.falsified_invariants++;
      if (invs.size() == 0) {
        no_invariants = true;
        parent.removeView(this);
        // for good measure; shouldn't be necessary, but just in case there
        // is some other pointer to this.
        clear_cache();
      }
    }
  }

  // I could make this more efficient, but it's probably fine as it is.
  public void removeInvariants(Vector to_remove) {
    for (int i=0; i<to_remove.size(); i++) {
      removeInvariant((Invariant) to_remove.elementAt(i));
    }
  }

  void addView(Ppt slice) {
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
  public abstract int num_samples();
  public abstract int num_mod_non_missing_samples();
  public abstract int num_values();
  public abstract String tuplemod_samples_summary();

  boolean check_modbits () {
    Assert.assert(! no_invariants);
    if (num_mod_non_missing_samples() < num_values()) {
      String message = "Bad mod bits in dtrace file:" + lineSep
        + "num_mod_non_missing_samples()=" + num_mod_non_missing_samples()
        + ", num_samples()=" + num_samples()
        + ", num_values()=" + num_values() + lineSep
        + "for " + name + lineSep
        + tuplemod_samples_summary() + lineSep
        + "Consider running modbit-munge.pl" + lineSep
        + ((values_cache == null)
           ? "Values cache has been cleared" + lineSep
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

  /**
     This class is used for comparing PptSlice objects.
     It orders by arity, then by variable names.
     It's somewhat less efficient than ArityPptnameComparator.
   **/
  public static final class ArityVarnameComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      PptSlice slice1 = (PptSlice) o1;
      PptSlice slice2 = (PptSlice) o2;
      // Don't do this, to permit comparison across different Ppts.
      // (The check may be useful in some situations, though.)
      // Assert.assert(slice1.parent == slice2.parent);
      if (slice1.arity == slice2.arity) {
        return slice1.varNames(slice1.var_infos)
          .compareTo(slice2.varNames(slice2.var_infos));
      } else {
        return slice2.arity - slice1.arity;
      }
    }
  }

  /**
     This class is used for comparing PptSlice objects.
     It orders by arity, then by name.
     Because of the dependence on name, it should be used only for slices
     on the same Ppt.
   **/
  public static final class ArityPptnameComparator implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      PptSlice slice1 = (PptSlice) o1;
      PptSlice slice2 = (PptSlice) o2;
      // Don't do this, to permit comparison across different Ppts.
      // (The check may be useful in some situations, though.)
      // Assert.assert(slice1.parent == slice2.parent);
      if (slice1.arity == slice2.arity) {
        return slice1.name.compareTo(slice2.name);
      } else {
        return slice2.arity - slice1.arity;
      }
    }
  }

}
