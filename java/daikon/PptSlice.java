package daikon;

import daikon.inv.*;

import java.util.logging.Logger;
import java.util.logging.Level;

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

  /** Debug tracer **/
  public static final Logger debug = Logger.getLogger("daikon.PptSlice");

  /** Debug tracer for debugging both this and PptSlices **/
  public static final Logger debugGeneral = Logger.getLogger("daikon.PptSlice.general");
  public static final Logger debugFlow = Logger.getLogger("daikon.flow.flow");

  public static final Logger debugGuarding = Logger.getLogger("daikon.guard");

  /** This is a slice of the 'parent' ppt. */
  public PptTopLevel parent;
  public abstract int arity();

  /**
   * The invariants contained in this slice.
   * This should not be used directly, in general.  In particular,
   * subclasses such as PptSlice0 need to synchronize it with other values.
   * Therefore, it should be manipulated via addInvariant() and
   * removeInvariant().
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
  private final Collection private_po_lower;
  // Map[PptTopLevel -> List[VarInfo[arity]]]; store as the value an array
  // where this.var_infos corresponds to the VarInfos in value.
  private final Map private_po_lower_vis;

  /**
   * Slices immediately lower in the partial order (compared to this).
   * If A is higher than B then every value seen at B is seen at A.
   * Elements are PptTopLevels, since PptSlices are transient.
   * Contains no duplicates.
   * PptSlice partial ordering is a direct function of VarInfo partial
   * ordering, and is the minimal nearest set of slices which are
   * higher for all VarInfos.
   **/
  public final Collection po_lower;
  public final Map po_lower_vis;

  /* [INCR]
  // These are used only when the values_cache has been set to null.
  int num_samples_post_cache = -2222;
  int num_mod_samples_post_cache = -2222;
  int num_values_post_cache = -2222;
  */

  /* [INCR] ...
  // This is rather a hack and should be removed later.
  // True if we've seen all values and are performing add() based on values
  // already in the values_cache; so add() should not add its arguments to
  // values_cache.
  public boolean already_seen_all = false;
  //... [INCR] */


  PptSlice(PptTopLevel parent, VarInfo[] var_infos) {
    this.parent = parent;
    this.var_infos = var_infos;
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i=0; i<var_infos.length-1; i++) {
      Assert.assertTrue(var_infos[i].varinfo_index <= var_infos[i+1].varinfo_index);
    }
    Assert.assertTrue(this instanceof PptSliceEquality || arity() == var_infos.length);
    invs = new Invariants();
    // These seem to be used by both top-down and bottom-up (with global point).

    if (! Daikon.dkconfig_df_bottom_up) {
      invs_to_flow = new Invariants(1);
      invs_changed = new Invariants(1);
      private_po_lower = new ArrayList(2);
      private_po_lower_vis = new HashMap();
      po_lower = Collections.unmodifiableCollection(private_po_lower);
      po_lower_vis = Collections.unmodifiableMap(private_po_lower_vis);
    } else {
      private_po_lower = null;
      private_po_lower_vis = null;
      po_lower = null;
      po_lower_vis = null;
    }

    if (debugGeneral.isLoggable(Level.FINE)) {
      debugGeneral.fine (ArraysMDE.toString(var_infos));
    }
  }

  /** Trim the collections used in this PptSlice. **/
  public void trimToSize() {
    super.trimToSize();
    invs.trimToSize();
  }

  public final String name() {
    return parent.name + varNames(var_infos);
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
   * @return true if any of our variables is named NAME, or is derived
   * from a variable named NAME.
   **/
  // Only called right now from tools/ExtractConsequent
  public boolean usesVarDerived(String name) {
    for (int i=0; i<var_infos.length; i++) {
      if (var_infos[i].name.includesSimpleName(name))
        return true;
    }
    return false;
  }

  /**
   * Falsifies the Invariant inv and then adds inv
   * to the list of Invariants that are to be flowed
   * and the list of changed Invariants.
   */
  public void destroyAndFlowInv(Invariant inv) {
    inv.falsify();
    addToFlow(inv);
    if (inv.logOn() || debugFlow.isLoggable(Level.FINE)) {
      inv.log (debugFlow, "Destroyed " + inv.format());
    }
    if (PrintInvariants.print_discarded_invariants) {
      parent.falsified_invars.add(inv);
    }
    addToChanged(inv);
  }

  /**
   * Places the Invariant inv to the list of
   * changed Invariants and adds the Invariant
   * clone to the list of Invariants that are to
   * be flowed.
   */
  public void flowClone(Invariant inv, Invariant clone) {
    addToChanged(inv);
    if (! inv.flowed) {
      addToFlow(clone);
      clone.flowed = true;
    }
  }


  /** @return true if all of this slice's variables are orig() variables. */
  public boolean allPrestate() {
    for (int i = 0; i < var_infos.length; i++) {
      if (!var_infos[i].isPrestateDerived())
        return false;
    }
    return true;
  }


  /**
   * Adds the given "slice" as one that is immediately lower in the
   * partial ordering, thus modifying po_lower and po_lower_vis.  The
   * argument is not a PptSlice because slices come and go over time.
   * Instead, it is specified as a PptTopLevel a subset of its
   * variables.
   *
   * @see #po_lower
   * @see #po_lower_vis
   **/
  protected void addToOnePO(PptTopLevel adj,
                            VarInfo[] slice_vis)
  {
    Assert.assertTrue(slice_vis.length == arity());
    for (int i = 0; i < arity(); i++) {
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

    if (Debug.logDetail())
      log ("Removing invariant '" + inv.format() + "'");
    boolean removed = invs.remove(inv);
    if (Assert.enabled && !removed)
      Assert.assertTrue (removed, "inv " + inv + " not in ppt " + name());
    // This increment could also have been in Invariant.destroy().
    Global.falsified_invariants++;
    if (invs.size() == 0) {
      if (Debug.logDetail())
        log ("last invariant removed");
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
   * @return the List of weakened invariants.
   **/
  protected List flow_and_remove_falsified() {
    // repCheck();  // Can do, but commented out for performance

    Assert.assertTrue (!Daikon.dkconfig_df_bottom_up);

    // Remove the dead invariants
    ArrayList to_remove = new ArrayList();
    for (Iterator iFalsified = invs.iterator(); iFalsified.hasNext(); ) {
      Invariant inv = (Invariant) iFalsified.next();
      if (inv.is_false()) {
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
    if (invs_changed.size() < invs_to_flow.size()) {
      throw new RuntimeException
        ("Changed Invariants count must equal to or greater than flowed invariants count");
    }

    removeInvariants(to_remove);

    // Flow newly-generated stuff
    if (invs_to_flow.size() == 0) {
      if (debugFlow.isLoggable(Level.FINE)) {
        debugFlow.fine ("No invariants to flow for " + this);
      }
      List result = new ArrayList (invs_changed);
      return result;
    } else {
      if (debugFlow.isLoggable(Level.FINE)) {
        debugFlow.fine (">> Flowing and removing falsified for: " + this);
        debugFlow.fine ("  To remove: " + to_remove);
        debugFlow.fine ("  To flow: " + invs_to_flow);
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

        if (Daikon.use_equality_optimization) {
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

        // Ensure the slice exists.
        PptSlice slice = lower.get_or_instantiate_slice(slice_vis);
        // slice.repCheck();  // Can do, but commented out for performance

        // Compute the permutation
        int[] permutation = new int[slice.arity()];
        // Do the permutation to map variables from this to lower slice
        for (int i=0; i < arity(); i++) {
          // slice.var_infos is small, so this call is relatively inexpensive
          permutation[i] = ArraysMDE.indexOf(slice.var_infos, slice_vis[i]);
        }
        fixPermutation (permutation);
        // For each invariant
      for_each_invariant:
        for (Iterator iInvsToFlow = invs_to_flow.iterator();
             iInvsToFlow.hasNext(); ) {
          Invariant inv = (Invariant) iInvsToFlow.next();
          if (! inv.is_false()) {
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
            inv.falsify();
          }
          // debug
          if (debugFlow.isLoggable(Level.FINE)) {
            debugFlow.fine (" " + inv.format() + " flowing from " +
                            parent.name + " to " + lower.name);
          }

          // Let it be reborn
          Invariant reborn = inv.resurrect(slice, permutation);
          if (debugFlow.isLoggable(Level.FINE)) {
            debugFlow.fine ("  rebirthing invariant");
          }

          // If its class does not already exist in lower
          // for (Iterator h = slice.invs.iterator(); h.hasNext(); ) {
          //   Object item = h.next();

          // Note that this must use reborn rather than the original
          // invariant.  That is because certain invariants will create
          // a new class when resurrecting (eg, switch from < to > because
          // their variable order switched).  We must check the reborn
          // invariant and not the previous one so we get the correct
          // match.

          // XXX Should this be some sort of same formula check
          // instead?  Probably not; one class of invariant should
          // be able to handle all data fed to it; we never need two
          // invariants of the same class in the same pptslice.
          // Maybe add that as rep invariant up above?
          //            if (item.getClass() == inv.getClass()) {
          Invariant alreadyThere = Invariant.find (reborn.getClass(), slice);
          if (alreadyThere != null) {
            if (debugFlow.isLoggable(Level.FINE))
              debugFlow.fine ("  except it was already there: " + alreadyThere.format());
            continue for_each_invariant;
          }

          slice.addInvariant(reborn);
          // Attempt to suppress the new invariant in lower levels
          slice.parent.attemptSuppression(reborn, true);
        }
      }
    }
    List result = new ArrayList (invs_changed);

    invs_to_flow.clear();
    invs_changed.clear();
    return result;
  }

  /**
   * Removes any falsified invariants from our list
   */
  protected void remove_falsified () {

    // Remove the dead invariants
    for (Iterator iFalsified = invs.iterator(); iFalsified.hasNext(); ) {
      Invariant inv = (Invariant) iFalsified.next();
      if (inv.is_false()) {
        iFalsified.remove();
      }
    }
  }

  /**
   * Remove repeated entries in a permutation.  The repeats are a
   * consequence of equality optimization: a VarInfo may be a
   * destination more than once due to equality splitting.  The fix is
   * to, for each repeat, increment the value.  So 0, 0, 2 becomes 0,
   * 1, 2.
   **/
  private void fixPermutation (int[] permutation) {
    for (int i = 0; i < permutation.length; i++) {
      int count = 0;
      for (int j = 0; j < permutation.length; j++) {
        if (permutation[i] == permutation[j]) {
          permutation[j] += count;
          count++;
        }
      }
    }
    Assert.assertTrue(ArraysMDE.fn_is_permutation(permutation));
  }

  void addSlice(Ppt slice) {
    throw new Error("Don't add views on a slice.");
  }

  void addSlices(Vector slices) {
    throw new Error("Don't add views on a slice.");
  }

  void removeSlice(Ppt slice) {
    throw new Error("Don't remove view from a slice.");
  }

  /**
   * Set the number of samples of this ppt to be at least count.
   **/
  public void set_samples (int count) {

  }

  // These accessors are for abstract methods declared in Ppt
  public abstract int num_samples();
  // public abstract int num_mod_samples();
  public abstract int num_values();

  boolean check_modbits () {
    Assert.assertTrue(invs.size() > 0);
    /* [INCR] (we no longer track num_values)
    if (num_mod_samples() < num_values()) {
      String message = "Bad mod bits in dtrace file:" + lineSep
        + "num_mod_samples()=" + num_mod_samples()
        + ", num_samples()=" + num_samples()
        + ", num_values()=" + num_values() + lineSep
        + "for " + name() + lineSep
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
   **/
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
      // Assert.assertTrue(slice1.parent == slice2.parent);
      if (slice1.arity() != slice2.arity()) {
        return slice2.arity() - slice1.arity();
      }
      return Ppt.varNames(slice1.var_infos)
        .compareTo(Ppt.varNames(slice2.var_infos));
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
      if (slice1.arity() != slice2.arity()) {
        return slice2.arity() - slice1.arity();
      }
      return slice1.name().compareTo(slice2.name());
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Invariant guarding

  // This function guards all of the invariants in a given PptSlice by
  // iterating over the contained invariants and replace the invariants
  // that require guarding with their guarded counterparts. The guarded
  // invariants are put into the joiner view of the PptTopLevel that
  // contains the PptSlice where the invariant was originally located.
  public void guardInvariants() {
    List invariantsToGuard = new ArrayList();

    if (debugGuarding.isLoggable(Level.FINE)) {
      debugGuarding.fine ("PptSlice.guardInvariants init: " + this.parent.name());
      debugGuarding.fine ("  I have " + invs.size() + " invariants");
      for (int i=0; i<var_infos.length; i++) {
        try {
          debugGuarding.fine ("  var_info[" + i +
                              "] name = " +
                              var_infos[i].name.name());
        } catch (UnsupportedOperationException e) {
          debugGuarding.fine ("  Part of PptSlice cannot be formatted.");
        }
      }
      // debugGuarding.fine ("In guardInvariants, the VarInfos for the PptSlice: ");
      // debugGuarding.fine (Arrays.asList(var_infos).toString());
    }

    // If this slice is to be deleted, then don't guard it
    if (invs.size() == 0) return;

    for (Iterator overInvs = invs.iterator(); overInvs.hasNext(); ) {
      Invariant inv = (Invariant)overInvs.next();
      if (debugGuarding.isLoggable(Level.FINE)) {
        debugGuarding.fine ("  Trying to add implication for: " + inv.repr());
      }
      if (inv.isGuardingPredicate) {
        debugGuarding.fine ("  Continuing: this is a guarding predicate");
        continue;
      }
      Invariant guardingPredicate = inv.createGuardingPredicate();
      Implication guardingImplication;
      if (debugGuarding.isLoggable(Level.FINE)) {
        if (guardingPredicate != null) {
          debugGuarding.fine ("  Predicate: " +
                              guardingPredicate.format());
          debugGuarding.fine ("  Consequent: " +
                              inv.format());
        } else {
          debugGuarding.fine ("  No implication needed");
        }
      }

      if (guardingPredicate != null) {
        guardingImplication =
          GuardingImplication.makeGuardingImplication(parent, guardingPredicate, inv, false);

        if (! parent.joiner_view.hasImplication(guardingImplication)) {
          parent.joiner_view.addInvariant(guardingImplication);
          invariantsToGuard.add(inv);

          if (debugGuarding.isLoggable(Level.FINE)) {
            debugGuarding.fine ("Adding " +
                                guardingImplication.format());
            debugGuarding.fine ("Removing " +
                                inv.format());
          }
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

  public void processOmissions(boolean[] omitTypes) {
    if (invs.size() == 0) return;
    List toRemove = new ArrayList();
    for (Iterator overInvs = invs.iterator(); overInvs.hasNext(); ) {
      Invariant inv = (Invariant)overInvs.next();
      if (omitTypes['r'] && inv.isReflexive())
        toRemove.add(inv);
      else if (omitTypes['s'] && inv.getSuppressor() != null)
        toRemove.add(inv);
    }
    removeInvariants(toRemove);
  }

  public void repCheck() {

    if (! Daikon.dkconfig_df_bottom_up) {
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
    }

    for (int i = 0; i < var_infos.length; i++) {
      for (int j = i+1; j < var_infos.length; j++) {
        // Assert.assertTrue (var_infos[i] != var_infos[j]);
      }
    }
    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant inv = (Invariant) i.next();
      inv.repCheck();
      Assert.assertTrue (inv.ppt == this);
    }
  }

  /**
   * Clone self and replace this.var_infos with newVis.  Do the same
   * in all invariants that this holds.  Return a new PptSlice that's
   * like this except with the above replacement, along with correct
   * flow pointers for varInfos.  Invariants are also pivoted so that
   * any VarInfo index order swapping is handled correctly.
   *
   * @param newVis to replace this.var_infos.
   * @return a new PptSlice that satisfies the characteristics above.
   **/
  PptSlice cloneAndPivot(VarInfo[] newVis) {
    throw new Error("Shouldn't get called");
  }

  public PptSlice copy_new_invs (PptTopLevel ppt, VarInfo[] vis) {
    throw new Error("Shouldn't get called");
  }

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
  /**
   * Returns whether or not this slice already contains the specified
   * invariant.  Whether not invariants match is determine by Invariant.match()
   * This will return true for invariants of the same kind with different
   * formulas (eg, one_of, bound, linearbinary)
   */
  public boolean contains_inv (Invariant inv) {

    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant mine = (Invariant) i.next();
      if (mine.match (inv))
        return (true);
    }
    return (false);
  }

  /**
   * Returns whether or not this slice contains an exact match
   * for the specified invariant.  An exact match requires that the
   * invariants be of the same class and have the same formula
   */
  public boolean contains_inv_exact (Invariant inv) {

    return (find_inv_exact(inv) != null);
  }

  /**
   * Returns the invariant that matches the specified invariant if it
   * exists.  Otherwise returns null.  An exact match requires that
   * the invariants be of the same class and have the same formula
   */
  public Invariant find_inv_exact (Invariant inv) {

    for (Iterator i = invs.iterator(); i.hasNext(); ) {
      Invariant mine = (Invariant) i.next();
      if ((mine.getClass() == inv.getClass()) && mine.isSameFormula(inv))
        return (mine);
    }
    return (null);
  }


  public void log (String msg) {
    Debug.log (getClass(), this, msg);
  }

  /**
   * Finds the global slice that corresponds the the slice identified
   * by local_vis.  Returns null if there is no corresponding slice.
   * Note that each local variable must have the same transform to the
   * global ppt for there to be a matching global slice.
   **/
  public PptSlice find_global_slice (VarInfo[] local_vis) {

    // Each var must have the same global transform in order for there
    // to be a matching global slice
    boolean post_xform = local_vis[0].is_post_global();
    if (!local_vis[0].is_global())
      return (null);
    for (int i = 1; i < local_vis.length; i++) {
      if (!local_vis[i].is_global()
          || (post_xform != local_vis[i].is_post_global()))
        return (null);
    }

    // Transform each local to the global ppt.  The global must be
    // a leader if the local one is.
    VarInfo[] global_vis = new VarInfo[local_vis.length];
    for (int i = 0; i < local_vis.length; i++) {
      global_vis[i] = local_vis[i].global_var();
      Assert.assertTrue (global_vis[i].isCanonical());
    }

    // As long as the variables are in the same order at the Global ppt,
    // there should be no permutation necessary between the global
    // and local slice (and thus global_vis should already be sorted)
    for (int i = 0; i < arity() - 1; i++) {
      if (global_vis[i].varinfo_index > global_vis[i+1].varinfo_index) {
        System.out.println ("localvars = " + VarInfo.toString(local_vis));
        System.out.println ("Globalvars = " + VarInfo.toString(global_vis));
        for (int j = 0; j < arity(); j++)
          System.out.print (local_vis[j].varinfo_index + "/"
                            + global_vis[j].varinfo_index + " ");
        System.out.println ();
        Assert.assertTrue (global_vis[i].varinfo_index
                           <= global_vis[i+1].varinfo_index);
      }
    }

    // Look for this slice at the global ppt and return it
    PptSlice slice = PptTopLevel.global.findSlice (global_vis);
    return (slice);
  }

}
