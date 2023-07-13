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
 * Indicates that the value of a string variable is always a time in 24-hour format, including
 * seconds. Prints as {@code x is Hour: HH:MM:SS 24-hour format with optional leading 0}.
 */
public class IsHourWithSeconds extends SingleString {
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  private static Pattern pattern = Pattern.compile("^(?:\\d|[01]\\d|2[0-3]):(?:[0-5]\\d):(?:[0-5]\\d)$");

  ///
  /// Required methods
  ///
  private IsHourWithSeconds(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype IsHourWithSeconds() {
    super();
  }

  private static @Prototype IsHourWithSeconds proto = new @Prototype IsHourWithSeconds();

  // Returns the prototype invariant
  public static @Prototype IsHourWithSeconds get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsHourWithSeconds instantiate_dyn(@Prototype IsHourWithSeconds this, PptSlice slice) {
    return new IsHourWithSeconds(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsHourWithSeconds this, OutputFormat format) {
    return var().name() + " is Hour: HH:MM:SS 24-hour format with optional leading 0";
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
    assert other instanceof IsHourWithSeconds;
    return true;
  }
}
