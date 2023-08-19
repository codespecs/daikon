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
 * Indicates that the value of a string variable is always a date following the format DD/MM/YYYY,
 * DD-MM-YYYY, or DD.MM.YYYY. Prints as {@code x is a Date. Format: DD/MM/YYYY}.
 */
public class IsDateDDMMYYYY extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff IsDateDDMMYYYY invariants should be considered. */
  public static boolean dkconfig_enabled = false;

  /**
   * The regex matches a date in DD/MM/YYYY format (Year min: 1900, Year max: 2050). For example:
   * "01/12/1900", "25.01.2019", "30-10-2050".
   */
  public static final Pattern PATTERN =
      Pattern.compile(
          ("^" + DAY_OF_MONTH + "/" + MONTH_NUMBER + "/" + YYYY2050 + "$")
              + "|"
              + ("^" + DAY_OF_MONTH + "-" + MONTH_NUMBER + "-" + YYYY2050 + "$")
              + "|"
              + ("^" + DAY_OF_MONTH + "[.]" + MONTH_NUMBER + "[.]" + YYYY2050 + "$"));

  ///
  /// Required methods
  ///

  /**
   * Creates a new IsDateDDMMYYYY.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsDateDDMMYYYY(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsDateDDMMYYYY. */
  private @Prototype IsDateDDMMYYYY() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsDateDDMMYYYY proto = new @Prototype IsDateDDMMYYYY();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsDateDDMMYYYY get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsDateDDMMYYYY instantiate_dyn(@Prototype IsDateDDMMYYYY this, PptSlice slice) {
    return new IsDateDDMMYYYY(slice);
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsDateDDMMYYYY this, OutputFormat format) {
    return var().name() + " is a Date. Format: DD/MM/YYYY";
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
    assert other instanceof IsDateDDMMYYYY;
    return true;
  }
}
