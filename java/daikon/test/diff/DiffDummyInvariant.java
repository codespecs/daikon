// A dummy invariant used for testing purposes

package daikon.test.diff;

import daikon.*;
import daikon.inv.*;
import java.util.List;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.NonPrototype;
import typequals.prototype.qual.Prototype;

/** A dummy invariant used for testing purposes. */
public class DiffDummyInvariant extends Invariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /** A string representation of the formula that this dummy invariant represents. */
  public String formula;

  /** The confidence fro this dummy invariant. */
  public double confidence;

  /** Whether the invariant is worth printing. */
  public boolean isWorthPrinting;

  /** Create an instance of DiffDummyInvariant. */
  public DiffDummyInvariant(PptSlice ppt, String formula, boolean justified) {
    this(ppt, formula, justified, true);
  }

  /** Create an instance of DiffDummyInvariant. */
  public DiffDummyInvariant(
      PptSlice ppt, String formula, boolean justified, boolean isWorthPrinting) {
    this(
        ppt,
        formula,
        (justified ? Invariant.CONFIDENCE_JUSTIFIED : Invariant.CONFIDENCE_UNJUSTIFIED),
        isWorthPrinting);
  }

  /** Create an instance of DiffDummyInvariant. */
  public DiffDummyInvariant(PptSlice ppt, String formula, double confidence) {
    this(ppt, formula, confidence, true);
  }

  /** Create an instance of DiffDummyInvariant. */
  public DiffDummyInvariant(
      PptSlice ppt, String formula, double confidence, boolean isWorthPrinting) {
    super(ppt);
    this.formula = formula;
    this.confidence = confidence;
    this.isWorthPrinting = isWorthPrinting;
  }

  @Override
  protected Invariant resurrect_done(int[] permutation) {
    throw new UnsupportedOperationException();
  }

  @Pure
  @Override
  public boolean isSameInvariant(Invariant other) {
    return this.isSameFormula(other);
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    if (other instanceof DiffDummyInvariant) {
      DiffDummyInvariant o = (DiffDummyInvariant) other;
      return this.formula.equals(o.formula);
    } else {
      return false;
    }
  }

  @Override
  public double computeConfidence() {
    return confidence;
  }

  @Override
  public String repr(@GuardSatisfied DiffDummyInvariant this) {
    return "DiffDummyInvariant(" + ppt.arity() + "," + formula + "," + confidence + ")";
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied DiffDummyInvariant this, OutputFormat format) {
    return repr();
  }

  // IsWorthPrinting should not be overridden by subclasses.
  // But this subclass is special:  it's not really an invariant,
  // but is only used for testing.
  @Pure
  @Override
  public boolean isWorthPrinting() {
    return isWorthPrinting;
  }

  @Override
  public boolean enabled() {
    throw new Error("do not invoke " + getClass() + ".enabled()");
  }

  @Override
  public boolean valid_types(VarInfo[] vis) {
    throw new Error("do not invoke " + getClass() + ".valid_types()");
  }

  @Override
  protected DiffDummyInvariant instantiate_dyn(PptSlice slice) {
    throw new Error("do not invoke " + getClass() + ".instantiate_dyn()");
  }

  @Override
  public @Nullable @NonPrototype DiffDummyInvariant merge(
      @Prototype DiffDummyInvariant this, List<@NonPrototype Invariant> invs, PptSlice parent_ppt) {
    throw new Error("do not merge DiffDummyInvariant");
  }
}
