package daikon;

import daikon.inv.*;

import org.apache.log4j.Category;

import java.util.*;

import utilMDE.*;

/**
 * This is a view on the full data (and maybe it does cacheing for a
 * while).  This will be efficient for iteration (albeit with
 * repetition), but inefficient for lookup (because we'd have to
 * iterate over all entries -- or at least all keys -- to find any
 * such instance, but we need to find them all to give a good result).
 **/

public abstract class PptSlice extends Ppt {
  public static final String lineSep = Global.lineSep;

  /**
   * Whether this particular program point is debugged.
   * Set via editing Globals.debuggedPptSliceSpecific
   **/
  public boolean debugged;

  /**
   * Logging Category.
   **/
  public static final Category debug = Category.getInstance(PptSlice.class.getName());
  public static final Category debugGeneral = Category.getInstance(PptSlice.class.getName() + ".general");

  /** This is a slice of the 'parent' ppt */
  public Ppt parent;
  public int arity;

  /**
   * Cache of values from var_infos, to avoid repeated lookups.
   * value_indices[i] == var_infos[i].value_index, but looking up in
   * this array should be cheaper than looking up in var_infos.
   * Has exactly this.arity elements.
   **/
  public int[] value_indices;

  /**
   * If true, then we are in the process of deleting this slice.
   * It should only be on a list of deferred to-be-deleted slices.
   * Thus, we should never see this non-false.
   **/
  public boolean no_invariants = false;

  /**
   * The invariants contained in this slice.
   **/
  public Invariants invs;

  // Keep private, modifiable copies and public read-only views
  private final Collection _po_higher = new ArrayList(2);
  private final Collection _po_lower = new ArrayList(2);
  // Map[PptSlice -> int[arity]]; store as the key a permutation array
  // where this.var_infos[i] corresponds to key.var_infos[value[i]].
  private final Map higher_permute = new HashMap();
  private final Map lower_permute = new HashMap();

  /**
   * Slices immediately higher in the partial order (compared to this).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are PptSlices.  Contains no duplicates.
   * PptSlice partial ordering is a direct function of VarInfo partial
   * ordering, and is the minimal nearest set of slices which are
   * higher for all VarInfos.
   * @see po_lower
   **/
  public final Collection po_higher = Collections.unmodifiableCollection(_po_higher);

  /**
   * Slices immediately lower in the partial order (compared to this).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are VarInfos.  Contains no duplicates.
   * PptSlice partial ordering is a direct function of VarInfo partial
   * ordering, and is the minimal nearest set of slices which are
   * higher for all VarInfos.
   * @see po_higher
   **/
  public final Collection po_lower = Collections.unmodifiableCollection(_po_lower);

  // This holds keys and elements of different types, depending on
  // he concrete child of PptSlice.
  // HashMap values_cache; // [INCR]

  /* [INCR]
  // These are used only when the values_cache has been set to null.
  int num_samples_post_cache = -2222;
  int num_mod_non_missing_samples_post_cache = -2222;
  int num_values_post_cache = -2222;
  String tuplemod_samples_summary_post_cache = "UNINITIALIZED";
  */

  /* [INCR] ...
  // This is rather a hack and should be removed later.
  // True if we've seen all values and are performing add() based on values
  // already in the values_cache; so add() should not add its arguments to
  // values_cache.
  public boolean already_seen_all = false;
  */ // ... [INCR]


  PptSlice(Ppt parent, VarInfo[] var_infos) {
    super(parent.name + varNames(var_infos));
    this.parent = parent;
    this.var_infos = var_infos;
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i=0; i<var_infos.length-1; i++) {
      Assert.assert(var_infos[i].varinfo_index < var_infos[i+1].varinfo_index);
    }
    arity = var_infos.length;
    value_indices = new int[arity];
    for (int i=0; i<arity; i++) {
      value_indices[i] = var_infos[i].value_index;
    }
    invs = new Invariants();

    // This comes after setting all other variables, as the function call may use name, arity, var_infos, etc.
    debugged = (Global.isDebuggedPptSlice(this));
    if (debugGeneral.isDebugEnabled()) {
      debugGeneral.debug(Arrays.asList(var_infos));
    }    
  }

  public boolean usesVar(VarInfo vi) {
    return (ArraysMDE.indexOfEq(var_infos, vi) != -1);
  }

  // This is only called from inv.filter.VariableFilter.
  public boolean usesVar(String name) {
    for (int i=0; i<var_infos.length; i++) {
      // mistere: I'm not sure is this is the right thing for
      // the gui, but it's probably close
      if (var_infos[i].name.name().equals(name)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Ensures that parent is in this.po_higher and this in in parent.po_lower.
   * Parent must be of the same arity.
   **/
  protected void addHigherPO(PptSlice parent,
			     int permutation[])
  {
    // Code copied in VarInfo; edit both copies.
    Assert.assert(this.arity == parent.arity);
    Assert.assert(permutation.length == arity);
    Assert.assert(ArraysMDE.is_permutation(permutation));

    if (this._po_higher.contains(parent)) {
      Assert.assert(parent._po_lower.contains(this));
      return;
    }
    Assert.assert(! parent._po_lower.contains(this));

    // clone for safety; intern for space
    int[] forward = Intern.intern((int[]) permutation.clone());
    int[] reverse = Intern.intern(ArraysMDE.inverse(forward));

    this._po_higher.add(parent);
    this.higher_permute.put(parent, forward);
    parent._po_lower.add(this);
    parent.lower_permute.put(this, reverse);
  }

  /* [INCR]
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
  */

  public abstract void addInvariant(Invariant inv);

  // I don't just use ppt.invs.remove because I want to be able to defer
  // and to take action if the vector becomes void.
  public void removeInvariant(Invariant inv) {
    Assert.assert(! no_invariants);
    Assert.assert(invs.contains(inv));
    boolean removed = invs.remove(inv);
    Assert.assert(removed);
    // This increment could also have been in Invariant.destroy().
    Global.falsified_invariants++;
    if (invs.size() == 0) {
      no_invariants = true;
    }
  }

  // I could make this more efficient, but it's probably fine as it is.
  public void removeInvariants(List to_remove) {
    for (int i=0; i<to_remove.size(); i++) {
      removeInvariant((Invariant) to_remove.get(i));
    }
  }

  /**
   * Use var_infos[].po_{higher,lower} to initialize this.po_{higher.lower}.
   * Discover the set of slices H such that:
   * <li> H.arity == this.arity
   * <li> exist i,j s.t. H.var_infos[i] :[ this.var_infos[j]  (higher in po)
   * <li> Not exist h1 in H, h2 in H s.t. path to h1 is prefix of path to h2  (minimality)
   **/
  abstract void init_po();

  /**
   * Flow falsified invariants to lower ppts, and remove them from
   * this ppt.
   **/
  void flow_and_remove_falsified() {
    // Build a worklist of invariants to flow
    List worklist = new ArrayList();
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      if (inv.no_invariant)
	worklist.add(inv);
    }
    // For each lower PptSlice
    for (Iterator j = _po_lower.iterator(); j.hasNext(); ) {
      PptSlice lower = (PptSlice) j.next();
      int[] permute = (int[]) lower_permute.get(lower);
      Assert.assert(permute != null);
      // For each invariant
      for (Iterator i = worklist.iterator(); i.hasNext(); ) {
	Invariant inv = (Invariant) i.next();
	Assert.assert(inv.no_invariant);
	// Let it be reborn
	Invariant reborn = inv.resurrect(lower, permute);
	lower.addInvariant(reborn);
      }
    }
    removeInvariants(worklist);
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

  /* [INCR]
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
  */

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
        // + ((values_cache == null)
        //    ? "Values cache has been cleared" + lineSep
        //    : "Values cache has not been cleared")
	;
      if (! Daikon.disable_modbit_check_message) {
        System.out.println(message);
	/* [INCR]
        if (values_cache != null) {
          System.out.println("To do:  Dump values_cache");
          // This is probably specific to the specializers of PptSlice.
          // values_cache.dump();
        }
	*/
      }
      if (! Daikon.disable_modbit_check_error)
        throw new Error(message);
    }
    return true;
  }

  abstract void instantiate_invariants();

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
