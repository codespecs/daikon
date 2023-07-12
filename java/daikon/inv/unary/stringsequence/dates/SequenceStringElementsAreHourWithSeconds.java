package daikon.inv.unary.stringsequence.dates;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.stringsequence.SingleStringSequence;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all elements of an array of strings are hours in 24-hour format, including
 * seconds. Prints as {@code All the elements of x are Hours: HH:MM:SS 24-hour format with optional
 * leading 0}.
 */
public class SequenceStringElementsAreHourWithSeconds extends SingleStringSequence {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = false;

  // Set to true if the array is empty. If we do not use this property, the invariant would be
  // considered true if all the arrays are empty
  private boolean alwaysEmpty = true;

  protected SequenceStringElementsAreHourWithSeconds(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SequenceStringElementsAreHourWithSeconds() {
    super();
  }

  private static @Prototype SequenceStringElementsAreHourWithSeconds proto =
      new @Prototype SequenceStringElementsAreHourWithSeconds();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype SequenceStringElementsAreHourWithSeconds get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreHourWithSeconds instantiate_dyn(
      @Prototype SequenceStringElementsAreHourWithSeconds this, PptSlice slice) {
    return new SequenceStringElementsAreHourWithSeconds(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreHourWithSeconds this) {
    return "SequenceStringElementsAreHourWithSeconds " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreHourWithSeconds this, OutputFormat format) {
    return "All the elements of "
        + var().name()
        + " are Hours: HH:MM:SS 24-hour format with optional leading 0";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {

    Pattern pattern = Pattern.compile("^(?:\\d|[01]\\d|2[0-3]):(?:[0-5]\\d):(?:[0-5]\\d)$");

    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      Matcher matcher = pattern.matcher(a[i]);
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

    if (alwaysEmpty) {
      return Invariant.CONFIDENCE_UNJUSTIFIED;
    }

    return 1 - Math.pow(.1, ppt.num_samples());
  }

  @Pure
  public @Nullable DiscardInfo isObviousImplied() {
    return null;
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof SequenceStringElementsAreHourWithSeconds;
    return true;
  }
}
