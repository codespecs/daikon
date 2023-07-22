package daikon.inv.unary.stringsequence.dates;

import daikon.PptSlice;
import daikon.inv.DiscardInfo;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.dates.IsTimeOfDay;
import daikon.inv.unary.stringsequence.SingleStringSequence;
import java.util.regex.Matcher;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that all elements of an array of strings are hours in 24-hour format. Prints as {@code
 * All the elements of x are TimeOfDays: HH:MM 24-hour format, optional leading 0}.
 */
public class SequenceStringElementsAreTimeOfDay extends SingleStringSequence {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = false;

  // Set to true if the array is empty. If we do not use this property, the invariant would be
  // considered true if all the arrays are empty
  private boolean alwaysEmpty = true;

  protected SequenceStringElementsAreTimeOfDay(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SequenceStringElementsAreTimeOfDay() {
    super();
  }

  private static @Prototype SequenceStringElementsAreTimeOfDay proto =
      new @Prototype SequenceStringElementsAreTimeOfDay();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype SequenceStringElementsAreTimeOfDay get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreTimeOfDay instantiate_dyn(
      @Prototype SequenceStringElementsAreTimeOfDay this, PptSlice slice) {
    return new SequenceStringElementsAreTimeOfDay(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreTimeOfDay this) {
    return "SequenceStringElementsAreTimeOfDay " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreTimeOfDay this, OutputFormat format) {
    return "All the elements of "
        + var().name()
        + " are TimeOfDays: HH:MM 24-hour format, optional leading 0";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {
    if (a.length > 0) {
      alwaysEmpty = false;
    }

    for (int i = 0; i < a.length; i++) {
      Matcher matcher = IsTimeOfDay.PATTERN.matcher(a[i]);
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
    assert other instanceof SequenceStringElementsAreTimeOfDay;
    return true;
  }
}
