package daikon;

import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.suppress.*;

import org.apache.log4j.Logger;

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
  public static final Logger debug = Logger.getLogger("daikon.PptSlice");

  /** Debug tracer for debugging both this and PptSlices **/
  public static final Logger debugGeneral = Logger.getLogger("daikon.PptSlice.general");
  public static final Logger debugFlow = Logger.getLogger("daikon.flow.flow");

  public static final Logger debugGuarding = Logger.getLogger("daikon.guard");


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


  /**
   * Invariants that have been falsified, and need to be flowed down
   * to lower ppts.  May not actually be a subset of invs, if certain
   * invariants fracture in two, for instance, or if an untainted
   * clone is flowed instead of the falsified invariant itself.
   **/
  private Invariants invs_to_flow;


  /**
   * Union of falsified and weakened invariants.  Cleared after every
   * call to flow_and_remove_falsified.
   **/
  private Invariants invs_changed;

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
      Assert.assertTrue(var_infos[i].varinfo_index < var_infos[i+1].varinfo_index);
    }
    arity = var_infos.length;
    value_indices = new int[arity];
    for (int i=0; i<arity; i++) {
      value_indices[i] = var_infos[i].value_index;
    }
    invs = new Invariants();
    invs_to_flow = new Invariants();
    invs_changed = new Invariants();

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

  private transient Dataflow.PptsAndInts controlling_cached = null;

  /**
   * @return true iff there is a slice higher in the PO relative to
   * this.  This call is relatively expensive.
   **/
  public boolean isControlled() {
    Dataflow.PptsAndInts controlling = getControlling();
    return controlling.ppts.length > 0;
  }

  /**
   * Return an array of PptsAndInts from which invariants can flow to
   * this.  This call is expensive but is cached.
   **/
  public Dataflow.PptsAndInts getControlling()
  {
    if (controlling_cached != null) return controlling_cached;


    Dataflow.PptsAndInts higher =
      Dataflow.compute_ppt_flow(parent,
                                var_infos,
                                false, // just one step
                                true   // higher
                                );

    List resultPpts = new LinkedList();
    List resultFlows = new LinkedList();

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
        resultPpts.add (higher.ppts[i]);
        resultFlows.add (flow);
      }
    }
    controlling_cached =
      new Dataflow.PptsAndInts ((PptTopLevel[]) resultPpts.toArray (new PptTopLevel[0]),
                                (int[][]) resultFlows.toArray (new int[0][]));
    return controlling_cached;
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
    Assert.assertTrue(slice_vis.length == arity);
    for (int i = 0; i < arity; i++) {
      Assert.assertTrue (slice_vis[i].type == this.var_infos[i].type);
    }


    // Collection private_po = lower ? private_po_lower : private_po_higher;
    // Map private_po_vis = lower ? private_po_lower_vis : private_po_higher_vis;
    Collection private_po = private_po_lower;
    Map private_po_vis = private_po_lower_vis;

    List slices;
    if (! private_po.contains(adj)) {
      private_po.add(adj);
      Assert.assertTrue(! private_po_vis.containsKey(adj));
      slices = new ArrayList(1);
      private_po_vis.put(adj, slices);
    } else {
      slices = (List) private_po_vis.get(adj);
      Assert.assertTrue(slices != null);
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
    Assert.assertTrue(invs_to_remove_deferred.size() == 0);
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
    Assert.assertTrue(! no_invariants);
    Assert.assertTrue(invs.contains(inv));
    boolean removed = invs.remove(inv);
    Assert.assertTrue(removed);
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
    invs_to_flow.add(inv);
  }

  /**
   * Place argument on worklist of invariants to flow when
   * flow_and_remove_falsified is called.  Should only be called from
   * Invariant objects as they are falsified.
   **/
  public void addToChanged(Invariant inv) {
    invs_changed.add(inv);
  }

  /**
   * This procedure accepts a sample (a ValueTuple), extracts the values
   * from it, casts them to the proper types, and passes them along to the
   * invariants proper.  (The invariants accept typed values rather than a
   * ValueTuple that encapsulates objects of any type whatever.)
   * @return a List of Invariants that weakened due to the processing.
   **/
  abstract List add (ValueTuple full_vt, int count);

  /**
   * Flow falsified invariants to lower ppts, and remove them from
   * this ppt.
   * @param invsFlowed After this method, holds the Invariants that
   * flowed.  Never null.
   * @return the List of weakened invariants.
   **/
  protected List flow_and_remove_falsified() {
    // repCheck();  // Can do, but commented out for performance

    // Remove the dead invariants
    ArrayList to_remove = new ArrayList();
    for (Iterator iFalsified = invs.iterator(); iFalsified.hasNext(); ) {
      Invariant inv = (Invariant) iFalsified.next();
      if (inv.falsified) {
        to_remove.add(inv);
      }
    }

    // The following is simply for error checking
    if (to_remove.size() > invs_to_flow.size()) {
      // This block may no longer be necessary, as destroy() is only
      // called via destroyAndFlow().

      // Could also assert that classes of invariants killed are all
      // represented in invs_to_flow, but a size check should be enough,
      // since most invariants only generate one flowed copy when they
      // die (and we call this method a lot, so let's not be wasteful).
      Set naughty = new HashSet(); // Classes that did not call
                                   // addToFlow after calling destroy.
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
    if (invs_changed.size() != invs_to_flow.size()) {
      throw new RuntimeException
        ("Changed Invariants count must equal flowed invariants count");
    }

    removeInvariants(to_remove);

    // Flow newly-generated stuff
    if (invs_to_flow.size() == 0) {
      if (debugFlow.isDebugEnabled()) {
        debugFlow.debug ("No invariants to flow for " + this);
      }
      return new ArrayList();
    } else {
      if (debugFlow.isDebugEnabled()) {
        debugFlow.debug (">> Flowing and removing falsified for: " + this);
        debugFlow.debug ("  To remove: " + to_remove);
        debugFlow.debug ("  To flow: " + invs_to_flow);
      }
    }


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
    for (Iterator iPptLower = po_lower.iterator(); iPptLower.hasNext(); ) {
      PptTopLevel lower = (PptTopLevel) iPptLower.next();
      // For all of the slices
      List slices_vis = (List) private_po_lower_vis.get(lower);
      for_each_slice:
      for (Iterator iLowerSlices = slices_vis.iterator();
           iLowerSlices.hasNext(); ) {
        VarInfo[] slice_vis = (VarInfo[]) iLowerSlices.next();

        for (int iSliceVis = 0; iSliceVis < slice_vis.length; iSliceVis++) {
          Assert.assertTrue(slice_vis[iSliceVis].type ==
                            this.var_infos[iSliceVis].type);
        }


        Assert.assertTrue (slice_vis.length == this.var_infos.length);

        if (Daikon.use_equality_set) {
          // Why clone?  Because we'll be doing clobbers to transform these
          // into leaders.
          VarInfo[] old_slice_vis = slice_vis;
          slice_vis = new VarInfo[slice_vis.length];
          System.arraycopy(old_slice_vis, 0, slice_vis, 0, slice_vis.length);
          // Convert the lower VarInfos into their leaders
          for (int iSliceVis = 0; iSliceVis < slice_vis.length; iSliceVis++) {
            slice_vis[iSliceVis] = (slice_vis[iSliceVis].equalitySet).leader();
            if (!slice_vis[iSliceVis].isCanonical()) {
              System.err.println ("Error, the variable " +
                                  slice_vis[iSliceVis].name.name() +
                                  " is not canonical");
              System.err.println ("in ppt " + this);
              throw new Error();
            }
          }
        }

        for (int iSliceVis = 0; iSliceVis < slice_vis.length; iSliceVis++) {
          Assert.assertTrue(slice_vis[iSliceVis].
                            comparableNWay (this.var_infos[iSliceVis]));
        }


        // Check because of equality.  If two VarInfos in this
        // PptSlice map to the same VarInfos in this PptSlice, we do
        // not flow the invariant.  Instead, when inequality is seen
        // at the lower's PptTopLevel, we instantiate slices.
        if (slice_vis.length >= 2) {
          if (slice_vis[0] == slice_vis[1]) {
            continue for_each_slice;
          }
          if (slice_vis.length >= 3) {
            if (slice_vis[2] == slice_vis[1] ||
                slice_vis[2] == slice_vis[0]) {
              continue for_each_slice;
            }
          }
        }

        // Ensure the slice exists.
        PptSlice slice = lower.get_or_instantiate_slice(slice_vis);
        // slice.repCheck();  // Can do, but commented out for performance

        // Compute the permutation
        int[] permutation = new int[slice.arity];
        // Do the permutation to map variables from this to lower slice
        for (int i=0; i < arity; i++) {
          // slice.var_infos is small, so this call is relatively inexpensive
          permutation[i] = ArraysMDE.indexOf(slice.var_infos, slice_vis[i]);
        }
        // For each invariant
      for_each_invariant:
        for (Iterator iInvsToFlow = invs_to_flow.iterator();
             iInvsToFlow.hasNext(); ) {
          Invariant inv = (Invariant) iInvsToFlow.next();
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
          if (debugFlow.isDebugEnabled()) {
            debugFlow.debug(" " + inv.format() + " flowing from " +
                            parent.name + " to " + lower.name);
          }
          // If its class does not already exist in lower
          // for (Iterator h = slice.invs.iterator(); h.hasNext(); ) {
          //   Object item = h.next();

            // XXX Should this be some sort of same formula check
            // instead?  Probably not; one class of invariant should
            // be able to handle all data fed to it; we never need two
            // invariants of the same class in the same pptslice.
            // Maybe add that as rep invariant up above?
          //            if (item.getClass() == inv.getClass()) {
          Invariant alreadyThere = Invariant.find (inv.getClass(), slice);
          if (alreadyThere != null) {
            if (debugFlow.isDebugEnabled())
              debugFlow.debug("  except it was already there: " + alreadyThere.format());
            continue for_each_invariant;
          }
          // Let it be reborn
          Invariant reborn = inv.resurrect(slice, permutation);
          if (debugFlow.isDebugEnabled()) {
            debugFlow.debug("  rebirthing invariant");
          }

          slice.addInvariant(reborn);
          // Attempt to suppress the new invariant in lower levels
          slice.parent.attemptSuppression(reborn);
        }
      }
    }
    List result = new ArrayList (invs_changed);
    invs_to_flow.clear();
    invs_changed.clear();
    return result;
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
    // Assert.assertTrue(check_modbits());

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

  /**
   * Set the number of samples of this ppt to be at least count.
   **/
  public void set_samples (int count) {

  }

  // These accessors are for abstract methods declared in Ppt
  public abstract int num_samples();
  public abstract int num_mod_non_missing_samples();
  // [INCR] public abstract int num_values();
  public abstract String tuplemod_samples_summary();

  boolean check_modbits () {
    Assert.assertTrue(! no_invariants);
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

  /**
   * Instantiate invariants on the VarInfos this slice contains.
   * @param excludeEquality This actually means "if true, avoid
   * instantiating invariants that are implied false because the
   * VarInfos on them were equal".  An example of this is IntLessThan.
   * If two VarInfos were previous equal and had non missing samples
   * for their slice, it would be unsound to instantiate IntLessThan
   * later.  excludeEquality is set to true when calling slices to
   * instantiate from PptSliceEquality, but false at PptTopLevel's
   * call.
   **/
  abstract void instantiate_invariants(boolean excludeEquality);

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
      // Assert.assertTrue(slice1.parent == slice2.parent);
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
      // Assert.assertTrue(slice1.parent == slice2.parent);
      if (slice1.arity == slice2.arity) {
        return slice1.name.compareTo(slice2.name);
      } else {
        return slice2.arity - slice1.arity;
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Invariant guarding

  // This function guards all of the invariants in a given PptSlice by
  // iterating over the contained invariants and replace the invariants
  // that require guarding with their guarded counterparts. The guarded
  // invariants are put into the joiner view of the PptTopLevel that
  // contains the PptSlice where the invariant was originally located
  public void guardInvariants() {
    List invariantsToGuard = new ArrayList();

    if (debugGuarding.isDebugEnabled()) {
      debugGuarding.debug("PptSlice.guardInvariants init: " + this.parent.ppt_name);
      debugGuarding.debug("  I have " + invs.size() + " invariants");
      for (int i=0; i<var_infos.length; i++) {
        try {
          debugGuarding.debug("  var_info[" + i +
                              "] name in JML = " +
                              var_infos[i].name.name_using(OutputFormat.JML));
        } catch (UnsupportedOperationException e) {
          debugGuarding.debug("  Part of PptSlice cannot be JML formatted.");
        }
      }
      //      debugGuarding.debug("In guardInvariants, the VarInfos for the PptSlice: ");
      //       debugGuarding.debug(Arrays.asList(var_infos).toString());
    }

    // If this slice is to be deleted, then don't guard it
    if (no_invariants) return;

    for (Iterator overInvs = invs.iterator(); overInvs.hasNext(); ) {
      Invariant inv = (Invariant)overInvs.next();
      if (debugGuarding.isDebugEnabled()) {
        debugGuarding.debug("  Trying to add implication for: " + inv.repr());
      }
      if (inv.isGuardingPredicate) {
        debugGuarding.debug("  Continuing: this is a guarding predicate");
        continue;
      }
      Invariant guardingPredicate = inv.createGuardingPredicate();
      Invariant guardingImplication;
      if (debugGuarding.isDebugEnabled()) {
        if (guardingPredicate != null) {
          debugGuarding.debug("  Predicate: " +
                              guardingPredicate.format_using(OutputFormat.JML));
          debugGuarding.debug("  Consequent: " +
                              inv.format_using(OutputFormat.JML));
        } else {
          debugGuarding.debug("  No implication needed");
        }
      }

      if (guardingPredicate != null) {
        guardingImplication =
          GuardingImplication.makeGuardingImplication(parent, guardingPredicate, inv, false);

        parent.joiner_view.addInvariant(guardingImplication);
        invariantsToGuard.add(inv);

        if (debugGuarding.isDebugEnabled()) {
          debugGuarding.debug("Adding " +
                           guardingImplication.format_using(OutputFormat.JML));
          debugGuarding.debug("Removing " +
                           inv.format_using(OutputFormat.JML));
        }
      }
    }

    removeInvariants(invariantsToGuard);
  }

  public boolean containsOnlyGuardingPredicates() {
    for (int i=0; i<invs.size(); i++) {
      if (!((Invariant)invs.get(i)).isGuardingPredicate)
        return false;
    }
    return true;
  }

  /////////////////////////////////////////////////////////////////
  /// Miscellaneous

  public void repCheck() {

    for (Iterator iPptLower = po_lower.iterator(); iPptLower.hasNext(); ) {
      PptTopLevel lower = (PptTopLevel) iPptLower.next();
      // For all of the slices
      List slices_vis = (List) private_po_lower_vis.get(lower);
      for_each_slice:

      for (Iterator iLowerSlices = slices_vis.iterator();
           iLowerSlices.hasNext(); ) {
        VarInfo[] slice_vis = (VarInfo[]) iLowerSlices.next();

        for (int iSliceVis = 0; iSliceVis < slice_vis.length; iSliceVis++) {
          if (slice_vis[iSliceVis].type !=
              this.var_infos[iSliceVis].type) {
            System.err.println ("RepCheck failure: " +
                                var_infos[iSliceVis].name.name() +
                                " and " +
                                slice_vis[iSliceVis].name.name() +
                                " have different types");
            System.err.println ("in ppt " + this);
            throw new Error();
          }
        }
      }
    }

    for (int i = 0; i < var_infos.length; i++) {
      for (int j = i+1; j < var_infos.length; j++) {
        Assert.assertTrue (var_infos[i] != var_infos[j]);
      }
    }
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      inv.repCheck();
      Assert.assertTrue (inv.ppt == this);
    }
  }

  // This method is used *during* inferencing to create invariants
  // that are checked.
  /**
   * Clone self and replace leader with newLeader.  Do the same in all
   * invariants that this holds.  Return a new PptSlice that's like
   * this except with the above replacement, along with correct flow
   * pointers for varInfos.  In this method, unlike cloneAllPivots,
   * this.var_infos are their leaders -- we are creating new
   * PptSlices for duplication during inferencing.
   * @pre leader is part of this.var_infos
   * @param leader The var to replace
   * @param newLeader The varInfos to replace (in order) each
   * occurance of leader.
   * @return a new PptSlice that satisfies the characteristics above.
   **/
  abstract PptSlice cloneOnePivot(VarInfo leader, VarInfo newLeader);

  // This method is used after inferencing, during postProcessing, to
  // pivot PptSlices to hold leaders only, after Equality sets have
  // themselves pivoted.
  /**
   * Clone self and replace non leader varInfos with leaders.  Do the
   * same in all invariants that this holds.  Return a new PptSlice
   * that's like this except with the above replacement, along with
   * correct flow pointers for varInfos.  In this method, unlike
   * cloneOnePivot, this.var_infos may not be their leaders.  We are
   * pivoting to restore this condition.
   * @pre this.var_infos members may not be their leaders
   * @return a new PptSlice that satisfies the characteristics above,
   * unless there is no need for pivoting, in which case we return
   * this.
   **/
  abstract PptSlice cloneAllPivots ();


  /**
   * For debugging only.
   **/
  public String toString() {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < var_infos.length; i++) {
      sb.append (" " + var_infos[i].name.name());
    }
    return this.getClass().getName() + ": " + parent.ppt_name + " " + sb + " samples: " + num_samples();
  }

}
