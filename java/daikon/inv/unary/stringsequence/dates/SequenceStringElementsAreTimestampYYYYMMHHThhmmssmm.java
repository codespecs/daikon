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
 * Indicates that all elements of an array of strings are timestamps. Prints as `All the elements of
 * x are Timestamps. Format: YYYY-MM-DDTHH:MM:SS.mmZ (Miliseconds are optional)`
 */
public class SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm extends SingleStringSequence {
x  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  public static boolean dkconfig_enabled = false;

  // Set to true if the array is empty. If we do not use this property, the invariant would be
  // considered true if all the arrays are empty
  private boolean alwaysEmpty = true;

  protected SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm() {
    super();
  }

  private static @Prototype SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm proto =
      new @Prototype SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm();

  /** Returns the prototype invariant for CommonStringSequence. */
  public static @Prototype SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm get_proto() {
    return proto;
  }

  /** returns whether or not this invariant is enabled */
  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  /** instantiate an invariant on the specified slice */
  @Override
  protected SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm instantiate_dyn(
      @Prototype SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm this, PptSlice slice) {
    return new SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm(slice);
  }

  // Don't write clone, because this.intersect is read-only
  // protected Object clone();

  @Override
  public String repr(@GuardSatisfied SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm this) {
    return "SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm " + varNames();
  }

  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm this,
      OutputFormat format) {
    return "All the elements of "
        + var().name()
        + " are Timestamp. Format: YYYY-MM-DDTHH:MM:SS.mmZ (Miliseconds are optional)";
  }

  @Override
  public InvariantStatus check_modified(@Interned String @Interned [] a, int count) {

    Pattern pattern =
        Pattern.compile(
            "^[0-9]{4}-((0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01])|(0[469]|11)-(0[1-9]|[12][0-9]|30)|(02)-(0[1-9]|[12][0-9]))T(0[0-9]|1[0-9]|2[0-3]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\\.[0-9]{3}){0,1}Z$");

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
    assert other instanceof SequenceStringElementsAreTimestampYYYYMMHHThhmmssmm;
    return true;
  }
}
