package daikon.inv.unary.stringsequence;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.IsNumeric;
import java.util.regex.Matcher;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the characters of all the elements of an array of strings are always numeric.
 * Prints as {@code All the elements of x are Numeric}.
 */
public class SequenceStringElementsAreNumeric extends SingleStringSequence {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequenceStringElementsAreNumeric invariants should be considered. */
  public static boolean dkconfig_enabled = true;

  /**
   * true if the array is always empty. Without this property, the invariant would be considered
   * true if all the arrays are empty
   */
  private boolean alwaysEmpty = true;

  /**
   * true if all the elements of the array are null. Without this property, the invariant would be
   * reported if all the arrays contain only null elements.
   */
  private boolean allElementsAreNull = true;

  /**
   * Creates a new SequenceStringElementsAreNumeric.
   *
   * @param ppt the slice with the variable of interest
   */
  protected SequenceStringElementsAreNumeric(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype SequenceStringElementsAreNumeric. */
  protected @Prototype SequenceStringElementsAreNumeric() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype SequenceStringElementsAreNumeric proto =
      new @Prototype SequenceStringElementsAreNumeric();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype SequenceStringElementsAreNumeric get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreNumeric instantiate_dyn(
      @Prototype SequenceStringElementsAreNumeric this, PptSlice slice) {
    return new SequenceStringElementsAreNumeric(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreNumeric this) {
    return "SequenceStringElementsAreNumeric " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreNumeric this, OutputFormat format) {
    return "All the elements of " + var().name() + " are Numeric";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      String arrayElement = a[i];
      if (arrayElement != null) {
        allElementsAreNull = false;
        Matcher matcher = IsNumeric.PATTERN.matcher(arrayElement);
        // The invariant is falsified if one of the elements of the array is NOT numeric
        if (!matcher.matches()) {
          return InvariantStatus.FALSIFIED;
        }
      }
    }

    return InvariantStatus.NO_CHANGE;
  }

  @Override
  public InvariantStatus add_modified(@Interned String @Interned [] a, int count) {
    return check_modified(a, count);
  }

  @Override
  protected double computeConfidence() {

    if (alwaysEmpty || allElementsAreNull) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }

    return 1 - Math.pow(.1, ppt.num_samples());
  }

  /**
   * DiscardInfo is not used for this invariant
   *
   * @return null
   */
  @Pure
  public @Nullable DiscardInfo isObviousImplied() {
    return null;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof SequenceStringElementsAreNumeric;
    return true;
  }
}
