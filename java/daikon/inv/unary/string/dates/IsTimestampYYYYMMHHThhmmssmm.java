package daikon.inv.unary.string.dates;

import static daikon.inv.unary.string.dates.DateRegexes.*;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.SingleString;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the value of a string variable is always a timestamp. Represented as x is
 * Timestamp. Prints as {@code YYYY-MM-DDTHH:MM:SS.mmZ (Millsiseconds are optional)}.
 */
public class IsTimestampYYYYMMHHThhmmssmm extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /** Matches a timestamp in the form {@code YYYY-MM-DDTHH:MM:SS.mmZ}. Milliseconds are optional. */
  public static final Pattern PATTERN =
      Pattern.compile(
          "^"
              + YYYY
              + "-"
              + MONTH_NUMBER
              + "-"
              + DAY_OF_MONTH
              + "T"
              + HH
              + ":"
              + MINUTES
              + ":"
              + SECONDS
              + mmmOptional
              + "Z"
              + "$");

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsTimestampYYYYMMHHThhmmssmm.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsTimestampYYYYMMHHThhmmssmm(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsTimestampYYYYMMHHThhmmssmm. */
  private @Prototype IsTimestampYYYYMMHHThhmmssmm() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsTimestampYYYYMMHHThhmmssmm proto =
      new @Prototype IsTimestampYYYYMMHHThhmmssmm();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsTimestampYYYYMMHHThhmmssmm get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsTimestampYYYYMMHHThhmmssmm instantiate_dyn(
      @Prototype IsTimestampYYYYMMHHThhmmssmm this, PptSlice slice) {
    return new IsTimestampYYYYMMHHThhmmssmm(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(
      @GuardSatisfied IsTimestampYYYYMMHHThhmmssmm this, OutputFormat format) {
    return var().name()
        + " is Timestamp. Format: YYYY-MM-DDTHH:MM:SS.mmZ (Milliseconds are optional)";
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    Matcher matcher = PATTERN.matcher(v);

    if (matcher.matches()) {
      return InvariantStatus.NO_CHANGE;
    }
    return InvariantStatus.FALSIFIED;
  }

  @Override
  public InvariantStatus add_modified(String v, int count) {
    return check_modified(v, count);
  }

  @Override
  protected double computeConfidence() {
    return 1 - Math.pow(.1, ppt.num_samples());
  }

  @Pure
  @Override
  public boolean isSameFormula(Invariant other) {
    assert other instanceof IsTimestampYYYYMMHHThhmmssmm;
    return true;
  }
}
