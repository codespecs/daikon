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
 * Indicates that the value of a string variable is always a time in 12-hour format. Prints as
 * {@code x is Hour: HH:MM 12-hour format, optional leading 0}.
 */
public class IsHourAMPM extends SingleString {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  ///
  /// Required methods
  ///
  private IsHourAMPM(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype IsHourAMPM() {
    super();
  }

  private static @Prototype IsHourAMPM proto = new @Prototype IsHourAMPM();

  // Returns the prototype invariant
  public static @Prototype IsHourAMPM get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsHourAMPM instantiate_dyn(@Prototype IsHourAMPM this, PptSlice slice) {
    return new IsHourAMPM(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsHourAMPM this, OutputFormat format) {
    return var().name()
        + " is Hour: HH:MM 12-hour format, optional leading 0, mandatory meridiems (AM/PM)";
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    Pattern pattern = Pattern.compile("^((1[0-2]|0?[1-9]):([0-5][0-9]) ?([AaPp][Mm]))$");

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
    assert other instanceof IsHourAMPM;
    return true;
  }
}
