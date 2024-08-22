package daikon;

import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.suppress.NIS;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;

/**
 * A Slice is a view of some of the variables for a program point. A program point (that is,
 * PptTopLevel) does not directly contain invariants. Instead, slices contain the invariants that
 * involve (all) the Slice's variables.
 *
 * <p>Suppose a program point has variables A, B, C, and D.<br>
 * There would be 4 unary slices -- one each for variables A, B, C, and D.<br>
 * There would be 6 binary slices -- for {A,B}, {A,C}, {A,D}, {B,C}, {B,D}, and {C,D}.<br>
 * There would be 4 ternary slices -- for {A,B,C}, {A,B,D}, {A,C,D}, and {B,C,D}.
 */
public abstract class PptSlice extends Ppt {
  static final long serialVersionUID = 20040921L;

  // Permit subclasses to use it.
  protected static final String lineSep = Global.lineSep;

  /** Debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.PptSlice");

  /** Debug tracer for debugging both this and PptSlices. */
  public static final Logger debugGeneral = Logger.getLogger("daikon.PptSlice.general");

  public static final Logger debugFlow = Logger.getLogger("daikon.flow.flow");

  public static final Logger debugGuarding = Logger.getLogger("daikon.guard");

  // A better name would perhaps be "container", as this has nothing to do
  // with the program point hierarchy.
  /** This is a slice of the 'parent' ppt. */
  public PptTopLevel parent;

  public abstract int arity(@UnknownInitialization(PptSlice.class) PptSlice this);

  /**
   * The invariants contained in this slice. This should not be used directly, in general. In
   * particular, subclasses such as PptSlice0 need to synchronize it with other values. Therefore,
   * it should be manipulated via {@link #addInvariant} and {@link #removeInvariant}.
   */
  @SuppressWarnings("serial")
  public List<Invariant> invs;

  PptSlice(PptTopLevel parent, VarInfo[] var_infos) {
    super(var_infos);
    this.parent = parent;
    invs = new ArrayList<Invariant>();
    // Ensure that the VarInfo objects are in order (and not duplicated).
    for (int i = 0; i < var_infos.length - 1; i++) {
      assert var_infos[i].varinfo_index <= var_infos[i + 1].varinfo_index;
    }
    assert this instanceof PptSliceEquality || arity() == var_infos.length;

    if (debugGeneral.isLoggable(Level.FINE)) {
      debugGeneral.fine(Arrays.toString(var_infos));
    }
  }

  @SideEffectFree
  @Override
  @SuppressWarnings("nullness:override.receiver") // see comment on overridden definition in Ppt
  public final String name(@GuardSatisfied @UnknownInitialization(PptSlice.class) PptSlice this) {
    return parent.name + varNames(var_infos);
  }

  public boolean usesVar(VarInfo vi) {
    return (ArraysPlume.indexOfEq(var_infos, vi) != -1);
  }

  // This is only called from inv.filter.VariableFilter.
  public boolean usesVar(String name) {
    for (int i = 0; i < var_infos.length; i++) {
      // mistere: I'm not sure is this is the right thing for
      // the gui, but it's probably close
      if (var_infos[i].name().equals(name)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if any of our variables is named NAME, or is derived from a variable named NAME.
   *
   * @return true if any of our variables is named NAME, or is derived from a variable named NAME
   */
  // Only called right now from tools/ExtractConsequent
  public boolean usesVarDerived(String name) {
    for (VarInfo vi : var_infos) {
      if (vi.includes_simple_name(name)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns true if all of this slice's variables are orig() variables.
   *
   * @return true if all of this slice's variables are orig() variables
   */
  public boolean allPrestate() {
    for (VarInfo vi : var_infos) {
      if (!vi.isPrestateDerived()) {
        return false;
      }
    }
    return true;
  }

  public abstract void addInvariant(Invariant inv);

  /** This method actually removes the invariant from its PptSlice. */
  // I don't just use ppt.invs.remove because I want to be able to defer
  // and to take action if the vector becomes void.
  public void removeInvariant(Invariant inv) {

    if (Debug.logDetail()) {
      log("Removing invariant '" + inv.format() + "'");
    }
    if (Debug.logOn()) {
      inv.log("Removed from slice: %s", inv.format());
    }
    boolean removed = invs.remove(inv);
    assert removed : "inv " + inv + " not in ppt " + name();
    Global.falsified_invariants++;
    if (invs.size() == 0) {
      if (Debug.logDetail()) {
        log("last invariant removed");
      }
    }
  }

  // This can be called with very long lists by the conditionals code.
  // At least until that's fixed, it's important for it not to be
  // quadratic.
  public void removeInvariants(List<Invariant> to_remove) {
    if (to_remove.size() < 10) {
      for (Invariant trinv : to_remove) {
        removeInvariant(trinv);
      }
    } else {
      int old_invs_size = invs.size();
      invs.removeAll(to_remove);
      assert old_invs_size - invs.size() == to_remove.size();
      Global.falsified_invariants += to_remove.size();
      if (invs.size() == 0) {
        if (Debug.logDetail()) {
          log("last invariant removed");
        }
      }
    }
  }

  /**
   * This procedure accepts a sample (a ValueTuple), extracts the values from it, casts them to the
   * proper types, and passes them along to the invariants proper. (The invariants accept typed
   * values rather than a ValueTuple that encapsulates objects of any type whatever.)
   *
   * @return a List of Invariants that weakened due to the processing
   */
  abstract List<Invariant> add(ValueTuple full_vt, int count);

  /** Removes any falsified invariants from our list. */
  @RequiresNonNull("daikon.suppress.NIS.suppressor_map")
  protected void remove_falsified() {

    // Remove the dead invariants
    for (Iterator<Invariant> iFalsified = invs.iterator(); iFalsified.hasNext(); ) {
      Invariant inv = iFalsified.next();
      if (inv.is_false()) {
        iFalsified.remove();
        NIS.falsified(inv);
      }
    }
  }

  /** Return an approximation of the number of samples seen on this slice. */
  public abstract int num_samples(@UnknownInitialization @GuardSatisfied PptSlice this);

  /** Return an approximation of the number of distinct values seen on this slice. */
  public abstract int num_values();

  /** Instantiate invariants on the VarInfos this slice contains. */
  abstract void instantiate_invariants();

  /**
   * This class is used for comparing PptSlice objects. It orders by arity, then by variable names.
   * It's somewhat less efficient than ArityPptnameComparator.
   */
  public static final class ArityVarnameComparator implements Comparator<PptSlice> {
    @Pure
    @Override
    public int compare(PptSlice slice1, PptSlice slice2) {
      if (slice1 == slice2) {
        return 0;
      }
      // Don't do this assert, which prevents comparison across different Ppts.
      // (The assert check may be useful in some situations, though.)
      // assert slice1.parent == slice2.parent;
      if (slice1.arity() != slice2.arity()) {
        return slice2.arity() - slice1.arity();
      }
      return Ppt.varNames(slice1.var_infos).compareTo(Ppt.varNames(slice2.var_infos));
    }
  }

  /**
   * This class is used for comparing PptSlice objects. It orders by arity, then by name. Because of
   * the dependence on name, it should be used only for slices on the same Ppt.
   */
  public static final class ArityPptnameComparator implements Comparator<PptSlice> {
    @Pure
    @Override
    public int compare(PptSlice slice1, PptSlice slice2) {
      if (slice1 == slice2) {
        return 0;
      }
      // Don't do this, to permit comparison across different Ppts.
      // (The check may be useful in some situations, though.)
      // assert slice1.parent == slice2.parent;
      if (slice1.arity() != slice2.arity()) {
        return slice2.arity() - slice1.arity();
      }
      return slice1.name().compareTo(slice2.name());
    }
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Invariant guarding

  /**
   * Returns true if every invariant is a guarding predicate.
   *
   * @return true if every invariant is a guarding predicate
   */
  public boolean containsOnlyGuardingPredicates() {
    for (Invariant inv : invs) {
      if (!inv.isGuardingPredicate) {
        return false;
      }
    }
    return true;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Miscellaneous

  /** Remove the invariants noted in omitTypes. */
  public void processOmissions(boolean[] omitTypes) {
    if (invs.size() == 0) {
      return;
    }
    List<Invariant> toRemove = new ArrayList<>();
    for (Invariant inv : invs) {
      if (omitTypes['r'] && inv.isReflexive()) {
        toRemove.add(inv);
      }
    }
    removeInvariants(toRemove);
  }

  /**
   * Check the internals of this slice. Each invariant in the slice is checked for consistency and
   * each inv.ppt must equal this.
   */
  public void repCheck() {

    // System.out.printf("Checking slice %s%n", this);

    // Make sure that each variable is a leader.  There is one exception to this
    // rule.  Post processing of equality sets creates equality invariants between the
    // various members of the equality set.  Thus one non-leader is acceptable
    // in binary (two variable) slices if it is in the same equality set as the
    // other variable.
    for (VarInfo vi : var_infos) {
      // System.out.printf("equality set for vi %s = %s%n", vi, vi.equalitySet);
      if (!vi.isCanonical()) {
        assert var_infos.length == 2 : this + " - " + vi;
        assert var_infos[0].canonicalRep() == var_infos[1].canonicalRep() : this + " - " + vi;
      }
    }

    for (Invariant inv : invs) {
      inv.repCheck();
      if (inv.ppt != this) {
        throw new Error(
            String.format(
                "inv.ppt != this.  inv.ppt=%s;  this=%s;  for inv=%s [%s]  in invs=%s",
                inv.ppt, this, inv, inv.getClass(), invs));
      }
    }
  }

  /**
   * Clone self and replace this.var_infos with newVis. Do the same in all invariants that this
   * holds. Return a new PptSlice that's like this except with the above replacement, along with
   * correct flow pointers for varInfos. Invariants are also pivoted so that any VarInfo index order
   * swapping is handled correctly.
   *
   * @param newVis to replace this.var_infos
   * @return a new PptSlice that satisfies the characteristics above
   */
  PptSlice cloneAndPivot(VarInfo[] newVis) {
    throw new Error("Shouldn't get called");
  }

  public PptSlice copy_new_invs(PptTopLevel ppt, VarInfo[] vis) {
    throw new Error("Shouldn't get called");
  }

  /** For debugging only. */
  @Override
  @SuppressWarnings("all:purity") // string creation
  @SideEffectFree
  public String toString(@GuardSatisfied PptSlice this) {
    StringBuilder sb = new StringBuilder();
    for (VarInfo vi : var_infos) {
      sb.append(" " + vi.name());
    }
    return this.getClass().getName()
        + ": "
        + parent.ppt_name
        // sb starts with a space
        + sb
        + " samples: "
        + num_samples();
  }

  /**
   * Returns whether or not this slice already contains the specified invariant. Whether not
   * invariants match is determine by Invariant.match() This will return true for invariants of the
   * same kind with different formulas (eg, one_of, bound, linearbinary).
   */
  public boolean contains_inv(Invariant inv) {

    for (Invariant mine : invs) {
      if (mine.match(inv)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Returns whether or not this slice contains an exact match for the specified invariant. An exact
   * match requires that the invariants be of the same class and have the same formula.
   */
  @EnsuresNonNullIf(result = true, expression = "find_inv_exact(#1)")
  public boolean contains_inv_exact(Invariant inv) {

    return (find_inv_exact(inv) != null);
  }

  /**
   * Returns the invariant that matches the specified invariant if it exists. Otherwise returns
   * null. An exact match requires that the invariants be of the same class and have the same
   * formula.
   */
  @Pure
  public @Nullable Invariant find_inv_exact(Invariant inv) {

    for (Invariant mine : invs) {
      if ((mine.getClass() == inv.getClass()) && mine.isSameFormula(inv)) {
        return mine;
      }
    }
    return null;
  }

  /**
   * Returns the invariant that matches the specified class if it exists. Otherwise returns null.
   */
  public @Nullable Invariant find_inv_by_class(Class<? extends Invariant> cls) {

    for (Invariant inv : invs) {
      if ((inv.getClass() == cls)) {
        return inv;
      }
    }
    return null;
  }

  /**
   * Returns true if the invariant is true in this slice. This can occur if the invariant exists in
   * this slice, is suppressed, or is obvious statically.
   */
  @Pure
  public boolean is_inv_true(Invariant inv) {

    if (contains_inv_exact(inv)) {
      if (Debug.logOn() && (Daikon.current_inv != null)) {
        Daikon.current_inv.log("inv %s exists", inv.format());
      }
      return true;
    }

    // Check to see if the invariant is obvious statically over the leaders.
    // This check should be sufficient since if it isn't obvious statically
    // over the leaders, it should have been created.
    DiscardInfo di = inv.isObviousStatically(var_infos);
    if (di != null) {
      if (Debug.logOn() && (Daikon.current_inv != null)) {
        Daikon.current_inv.log("inv %s is obv statically", inv.format());
      }
      return true;
    }

    boolean suppressed = inv.is_ni_suppressed();
    if (suppressed && Debug.logOn() && (Daikon.current_inv != null)) {
      Daikon.current_inv.log("inv %s is ni suppressed", inv.format());
    }
    return suppressed;
  }

  /**
   * Output specified log information if the PtpSlice class, and this ppt and variables are enabled
   * for logging.
   */
  public void log(String msg) {
    Debug.log(getClass(), this, msg);
  }
}
