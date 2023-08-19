package daikon.inv.unary.stringsequence.dates;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.dates.IsTimeOfDayWithSeconds;
import daikon.inv.unary.stringsequence.SingleStringSequence;
import java.util.regex.Matcher;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all elements of an array of strings are hours in 24-hour format, including
 * seconds. Prints as {@code All the elements of x are TimeOfDays: HH:MM:SS 24-hour format with
 * optional leading 0}.
 */
public class SequenceStringElementsAreTimeOfDayWithSeconds extends SingleStringSequence {

  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean. True iff SequenceStringElementsAreTimeOfDayWithSeconds invariants should be
   * considered.
   */
  public static boolean dkconfig_enabled = true;

  /**
   * true if the array is always empty. Without this property, the invariant would be considered
   * true if all the arrays are empty
   */
  private boolean alwaysEmpty = true;

  /**
   * Creates a new SequenceStringElementsAreTimeOfDayWithSeconds.
   *
   * @param ppt the slice with the variable of interest
   */
  protected SequenceStringElementsAreTimeOfDayWithSeconds(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype SequenceStringElementsAreTimeOfDayWithSeconds. */
  protected @Prototype SequenceStringElementsAreTimeOfDayWithSeconds() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype SequenceStringElementsAreTimeOfDayWithSeconds proto =
      new @Prototype SequenceStringElementsAreTimeOfDayWithSeconds();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype SequenceStringElementsAreTimeOfDayWithSeconds get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreTimeOfDayWithSeconds instantiate_dyn(
      @Prototype SequenceStringElementsAreTimeOfDayWithSeconds this, PptSlice slice) {
    return new SequenceStringElementsAreTimeOfDayWithSeconds(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreTimeOfDayWithSeconds this) {
    return "SequenceStringElementsAreTimeOfDayWithSeconds " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreTimeOfDayWithSeconds this, OutputFormat format) {
    return "All the elements of "
        + var().name()
        + " are TimeOfDays: HH:MM:SS 24-hour format with optional leading 0";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      Matcher matcher = IsTimeOfDayWithSeconds.PATTERN.matcher(a[i]);
      if (!matcher.matches()) {
        return InvariantStatus.FALSIFIED;
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
    assert other instanceof SequenceStringElementsAreTimeOfDayWithSeconds;
    return true;
  }
}
