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
 * Indicates that all the elements of an array of strings are dates following the format MM/DD/YYYY.
 * Represented as {@code All the elements of x are dates. Format: MM/DD/YYYY}.
 */
public class SequenceStringElementsAreDateMMDDYYYY extends SingleStringSequence {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = false;

  // Set to true if the array is empty. If we do not use this property, the invariant would be
  // considered true if all the arrays are empty
  private boolean alwaysEmpty = true;

  protected SequenceStringElementsAreDateMMDDYYYY(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SequenceStringElementsAreDateMMDDYYYY() {
    super();
  }

  private static @Prototype SequenceStringElementsAreDateMMDDYYYY proto =
      new @Prototype SequenceStringElementsAreDateMMDDYYYY();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype SequenceStringElementsAreDateMMDDYYYY get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreDateMMDDYYYY instantiate_dyn(
      @Prototype SequenceStringElementsAreDateMMDDYYYY this, PptSlice slice) {
    return new SequenceStringElementsAreDateMMDDYYYY(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreDateMMDDYYYY this) {
    return "SequenceStringElementsAreDateMMDDYYYY " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreDateMMDDYYYY this, OutputFormat format) {
    return "All the elements of " + var().name() + " are dates. Format: MM/DD/YYYY";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {

    /*
     *   The regex matches on a date with the MM/DD/YYYY format (Year min: 1900, Year max: 2050).
     *   For example:
     *       - 12/01/1900
     *       - 01.25.2019
     *       - 10-30-2050
     */
    // ^(?:0[1-9]|1[012])[-/.](?:0[1-9]|[12][0-9]|3[01])[-/.](?:19\d{2}|20[0134][0-9]|2050)$
    Pattern pattern =
        Pattern.compile(
            "^(?:0[1-9]|1[012])[-/.](?:0[1-9]|[12][0-9]|3[01])[-/.](?:19\\d{2}|20[0134][0-9]|2050)$");

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
    assert other instanceof SequenceStringElementsAreDateMMDDYYYY;
    return true;
  }
}
