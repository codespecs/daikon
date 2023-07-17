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
 * Indicates that the value of a string variable is always a date following the format MM/DD/YYYY,
 * MM-DD-YYYY, or MM.DD.YYYY. Prints as {@code x is a Date. Format: MM/DD/YYYY}.
 */
public class IsDateMMDDYYYY extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff Positive invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /**
   * The regex matches a date in MM/DD/YYYY format (Year min: 1900, Year max: 2050). For example:
   * "12/01/1900", "01.25.2019", "10-30-2050".
   */
  public static final Pattern PATTERN =
      Pattern.compile("^" + MONTH_NUMBER + "[-/.]" + DAY_OF_MONTH + "[-/.]" + YYYY2050 + "$");

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsDateMMDDYYYY.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsDateMMDDYYYY(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsDateMMDDYYYY. */
  private @Prototype IsDateMMDDYYYY() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsDateMMDDYYYY proto = new @Prototype IsDateMMDDYYYY();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
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
    assert other instanceof IsDateMMDDYYYY;
    return true;
  }
}
