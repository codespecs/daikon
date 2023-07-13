package daikon.inv.unary.string.dates;

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
 * Timestamp. Prints as {@code YYYY-MM-DDTHH:MM:SS.mmZ (Miliseconds are optional)}.
 */
public class IsTimestampYYYYMMHHThhmmssmm extends SingleString {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  private static Pattern pattern =
          Pattern.compile(
                  "^[0-9]{4}-((0[13578]|1[02])-(0[1-9]|[12][0-9]|3[01])|(0[469]|11)-(0[1-9]|[12][0-9]|30)|(02)-(0[1-9]|[12][0-9]))T(0[0-9]|1[0-9]|2[0-3]):(0[0-9]|[1-5][0-9]):(0[0-9]|[1-5][0-9])(\\.[0-9]{3}){0,1}Z$");

  ///
  /// Required methods
  ///
  private IsTimestampYYYYMMHHThhmmssmm(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype IsTimestampYYYYMMHHThhmmssmm() {
    super();
  }

  private static @Prototype IsTimestampYYYYMMHHThhmmssmm proto =
      new @Prototype IsTimestampYYYYMMHHThhmmssmm();

  // Returns the prototype invariant
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
        + " is Timestamp. Format: YYYY-MM-DDTHH:MM:SS.mmZ (Miliseconds are optional)";
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    Matcher matcher = pattern.matcher(v);

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
