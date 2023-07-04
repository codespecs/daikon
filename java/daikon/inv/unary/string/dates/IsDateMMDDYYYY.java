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
 * Indicates that the value of a string variable is always a date following the format MM/DD/YYYY
 * (the separator can be ``/'' or ``-''). Prints as {@code x is a Date. Format: MM/DD/YYYY}.
 */
public class IsDateMMDDYYYY extends SingleString {
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
  private IsDateMMDDYYYY(PptSlice ppt) {
    super(ppt);
  }

  private @Prototype IsDateMMDDYYYY() {
    super();
  }

  private static @Prototype IsDateMMDDYYYY proto = new @Prototype IsDateMMDDYYYY();

  // Returns the prototype invariant
  public static @Prototype IsDateMMDDYYYY get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsDateMMDDYYYY instantiate_dyn(@Prototype IsDateMMDDYYYY this, PptSlice slice) {
    return new IsDateMMDDYYYY(slice);
  }

  // A printed representation for user output
  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsDateMMDDYYYY this, OutputFormat format) {
    return var().name() + " is a Date. Format: MM/DD/YYYY";
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
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
    assert other instanceof IsDateMMDDYYYY;
    return true;
  }
}
