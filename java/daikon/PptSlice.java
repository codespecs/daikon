package daikon;

import daikon.inv.*;

import org.apache.log4j.Category;

import java.util.*;

import utilMDE.*;

/**
 * A Slice is a view of some of the variables for a program point.  A
 * program point (that is, PptTopLevel) does not directly contain
 * invariants.  Instead, slices contain the invariants that involve (all)
 * the Slice's variables.
 * <p>
 * Suppose a program point has variables A, B, C, and D.
 * There would be 4 unary slices -- one each for variables A, B, C, and D.
 * There would be 6 binary slices -- for {A,B}, {A,C}, {A,D}, {B,C}, {B,D},
 * and {C,D}.
 * There would be 4 ternary slices -- for {A,B,C}, {A,B,D}, {A,C,D}, and
 * {B,C,D}.
 **/

public abstract class PptSlice
  extends Ppt
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public static final String lineSep = Global.lineSep;

  /**
   * Whether this particular program point is debugged.
   * Set via editing Globals.debuggedPptSliceSpecific
   **/
  public boolean debugged;

  /** Debug tracer **/
  public static final Category debug = Category.getInstance("daikon.PptSlice");
  public static final Category debugGeneral = Category.getInstance("daikon.PptSlice.general");
  public static final Category debugFlow = Category.getInstance("daikon.PptSlice.flow");

  /** This is a slice of the 'parent' ppt. */
  public PptTopLevel parent;
  public int arity;

  /**
   * Cache of values from var_infos, to avoid repeated lookups.
   * value_indices[i] == var_infos[i].value_index, but looking up in
   * this array should be cheaper than looking up in var_infos.
   * Representation invariant: value_indices.length == this.arity.
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

  // Invariants that have been falsified, and need to be flowed down
  // to lower ppts.  May not actually be a subset of invs, if certain
  // invariants fracture in two, for instance, or if an untainted
  // clone is flowed instead of the falsified invariant itself.
  private Invariants invs_to_flow;

  // Keep private, modifiable copies and public read-only views
  private final Collection private_po_lower = new ArrayList(2);  // [PptTopLevel]
  // Map[PptTopLevel -> List[VarInfo[arity]]]; store as the value an array
  // where this.var_infos corresponds to the VarInfos in value.
  private final Map private_po_lower_vis = new HashMap();

  /**
   * Slices immediately lower in the partial order (compared to this).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are PptTopLevels, since PptSlices are transient.
   * Contains no duplicates.
   * PptSlice partial ordering is a direct function of VarInfo partial
   * ordering, and is the minimal nearest set of slices which are
   * higher for all VarInfos.
   **/
  public final Collection po_lower = Collections.unmodifiableCollection(private_po_lower);
  public final Map po_lower_vis = Collections.unmodifiableMap(private_po_lower_vis);

  // This holds keys (interned) and elements of different types, depending on
  // the concrete child of PptSlice.
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


  PptSlice(PptTopLevel parent, VarInfo[] var_infos) {
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
    invs_to_flow = new Invariants();

    // This comes after setting all other variables, as the function call may use name, arity, var_infos, etc.
    debugged = (Global.isDebuggedPptSlice(this));

    if (debugGeneral.isDebugEnabled()) {
      debugGeneral.debug(Arrays.asList(var_infos));
    }
  }

  /** Trim the collections used in this PptSlice. **/
  public void trimToSize() {
    super.trimToSize();
    invs.trimToSize();
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
   * @return true iff there is a slice higher in the PO relative to
   * this.  This call is relatively expensive.
   **/
  public boolean isControlled()
  {
    Dataflow.PptsAndInts higher =
      Dataflow.compute_ppt_flow(parent,
				var_infos,
				false, // just one step
				true   // higher
				);

    // We always have at least one path, since the dataflow result
    // includes 'here' as its last element -- we ignore the 'here'
    // path.  If any other path maps all variables from this program
    // point, we are controlled.

    final int all_except_here = higher.ppts.length - 1;
    for (int i = 0; i < all_except_here; i++) {
      int[] flow = higher.ints[i];
      boolean all = true;       // below, all controls whether to return
      for (int j = 0; all && (j < arity); j++) {
	int varinfo_index = var_infos[j].varinfo_index;
	int var_flow_to = flow[varinfo_index];
	boolean flows = (var_flow_to != -1);
	all = all && flows;
      }
      if (Global.debugInfer.isDebugEnabled()) {
	Global.debugInfer.debug
	  ("isControlled: "
	   + name + " controlled by " + higher.ppts[i].name + "? : "
	   + (all ? "yes" : "no"));
      }
      if (all) {
	return true;
      }
    }

    return false;
  }

  /**
   * Adds the given "slice" as one that is immediately lower in the
   * partial ordering, thus modifying po_lower and po_lower_vis.  The
   * argument is not a PptSlice because slices come and go over time.
   * Instead, it is specified as a PptTopLevel a subset of its
   * variables.
   *
   * @see po_lower, po_lower_vis
   **/
  protected void addToOnePO(PptTopLevel adj,
			    VarInfo[] slice_vis)
  {
    Assert.assert(slice_vis.length == arity);

    // Collection private_po = lower ? private_po_lower : private_po_higher;
    // Map private_po_vis = lower ? private_po_lower_vis : private_po_higher_vis;
    Collection private_po = private_po_lower;
    Map private_po_vis = private_po_lower_vis;

    List slices;
    if (! private_po.contains(adj)) {
      private_po.add(adj);
      Assert.assert(! private_po_vis.containsKey(adj));
      slices = new ArrayList(1);
      private_po_vis.put(adj, slices);
    } else {
      slices = (List) private_po_vis.get(adj);
      Assert.assert(slices != null);
    }
    slices.add(slice_vis);
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

  /** This method actually removes the invariant from its PptSlice. **/
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
      // System.out.println("Removing view " + this.name + " because removed last invariant " + inv.format());
    }
  }

  // I could make this more efficient, but it's probably fine as it is.
  public void removeInvariants(List to_remove) {
    for (int i=0; i<to_remove.size(); i++) {
      removeInvariant((Invariant) to_remove.get(i));
    }
  }

  /**
   * Place argument on worklist of invariants to flow when
   * flow_and_remove_falsified is called.  Should only be called from
   * Invariant objects as they are falsified.
   **/
  public void addToFlow(Invariant inv) {
    debugFlow.debug(inv.format() + " at " + parent.name + " added to flow");
    invs_to_flow.add(inv);
  }

  /**
   * Flow falsified invariants to lower ppts, and remove them from
   * this ppt.
   **/
  protected void flow_and_remove_falsified() {
    // Remove the dead invariants
    ArrayList to_remove = new ArrayList();
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      if (inv.falsified) {
	to_remove.add(inv);
      }
    }
    // Could also assert that classes of invariants killed are all
    // represented in invs_to_flow, but a size check should be enough,
    // since most invariants only generate one flowed copy when they
    // die (and we call this method a lot, so let's not be wasteful).
    if (to_remove.size() > invs_to_flow.size()) {
      Set naughty = new HashSet();
      for (Iterator i = to_remove.iterator(); i.hasNext(); ) {
	Invariant inv = (Invariant) i.next();
	naughty.add(inv.repr());
      }
      for (Iterator i = invs_to_flow.iterator(); i.hasNext(); ) {
	Invariant inv = (Invariant) i.next();
	naughty.remove(inv.repr());
      }
      throw new RuntimeException
	("Class(es) did not call addToFlow after calling destroy: " + naughty);
    }
    removeInvariants(to_remove);

    // Flow newly-generated stuff
    if (invs_to_flow.size() == 0) return;

    // XXXX Currently, we flow invariants to all immediately-lower
    // PptSlices.  If the same sample happens to follow them there,
    // then they are again falsified and flowed.  This is slightly
    // inefficient.  Worse, it leads to redundant invariants when an
    // invariant class can flow even when not falsified (like OneOf or
    // Bound invariants); find these by searching for calls to
    // flowClone().  The correct thing to do is to flow to all nearest
    // lower slices that are not covered by the sample being
    // processed.  Its actually even harder that that, because even
    // though the sample might flow to a PptTopLevel, it might not
    // flow to a PptConditional that hangs from it.

    // For each lower PptTopLevel
    for (Iterator j = po_lower.iterator(); j.hasNext(); ) {
      PptTopLevel lower = (PptTopLevel) j.next();
      // For all of the slices
      List slices_vis = (List) private_po_lower_vis.get(lower);
      for (Iterator k = slices_vis.iterator(); k.hasNext(); ) {
	VarInfo[] slice_vis = (VarInfo[]) k.next();
	// Ensure the slice exists.
	PptSlice slice = lower.get_or_instantiate_slice(slice_vis);
	// Compute the permutation
	int[] permutation = new int[slice.arity];
	for (int i=0; i < arity; i++) {
          // slice.var_infos is small, so this call is relatively inexpensive
	  permutation[i] = ArraysMDE.indexOf(slice.var_infos, slice_vis[i]);
	}
	// For each invariant
      for_each_invariant:
	for (Iterator i = invs_to_flow.iterator(); i.hasNext(); ) {
	  Invariant inv = (Invariant) i.next();
	  if (! inv.falsified) {
	    // The invariant must be destroyed before it can be
	    // resurrected.  The invariant objects that flow are
	    // provided by the invariants that are falsified or change
	    // formula.  Depending on the characteristics of the
	    // invariant, the item to flow may or may not have been
	    // destroyed.  Invariants that do not compute any
	    // constants will often just flow themselves directly,
	    // which means that inv will be falsified.  On the other
	    // hand, invariants with weaken-able computed constants
	    // will flow a clone of themselves before they weaken.  In
	    // that case, inv will not yet have been falsified.
	    inv.destroy();
	  }
	  // debug
	  if (debugFlow.isDebugEnabled()) debugFlow.debug(" " + inv.format() + " flowing from " + parent.name + " to " + lower.name);
	  // If its class does not already exist in lower
	  for (Iterator h = slice.invs.iterator(); h.hasNext(); ) {
	    Object item = h.next();
	    // XXX Should this be some sort of same formula check
	    // instead?  Probably not; one class of invariant should
	    // be able to handle all data fed to it; we never need two
	    // invariants of the same class in the same pptslice.
	    // Maybe add that as rep invariant up above?
	    if (item.getClass() == inv.getClass()) {
	      if (debugFlow.isDebugEnabled()) debugFlow.debug("  except it was already there");
	      continue for_each_invariant;
	    }
	  }
	  // Let it be reborn
	  Invariant reborn = inv.resurrect(slice, permutation);
	  slice.addInvariant(reborn);
	}
      }
    }
    invs_to_flow.clear();
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
  // [INCR] public abstract int num_values();
  public abstract String tuplemod_samples_summary();

  boolean check_modbits () {
    Assert.assert(! no_invariants);
    /* [INCR] (we no longer track num_values)
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
	// [INCR]
        if (values_cache != null) {
          System.out.println("To do:  Dump values_cache");
          // This is probably specific to the specializers of PptSlice.
          // values_cache.dump();
        }
      }
      if (! Daikon.disable_modbit_check_error)
        throw new Error(message);
    }
    */ // ... [INCR]
    return true;
  }

  abstract void instantiate_invariants();

  /**
   * This class is used for comparing PptSlice objects.
   * It orders by arity, then by variable names.
   * It's somewhat less efficient than ArityPptnameComparator.
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
   * This class is used for comparing PptSlice objects.
   * It orders by arity, then by name.
   * Because of the dependence on name, it should be used only for slices
   * on the same Ppt.
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
