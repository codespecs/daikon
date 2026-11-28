package daikon.inv.unary.string.dates;

import static daikon.inv.unary.string.dates.DateRegexes.*;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.string.SingleString;
import java.util.regex.Pattern;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import typequals.prototype.qual.Prototype;

/**
 * Indicates that the value of a string variable is always a date following the format YYYY/MM/DD,
 * YYYY-MM-DD, or YYYY.MM.DD. Prints as {@code x is a Date. Format: YYYY-MM-DD}.
 */
public class IsDateYYYYMMDD extends SingleString {
  /** UID for serialization. */
  static final long serialVersionUID = 20230704L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff IsDateYYYYMMDD invariants should be considered. */
  public static boolean dkconfig_enabled = Invariant.invariantEnabledDefault;

  /**
   * This regex matches a date in YYYY-MM-DD format (Year min: 1900, Year max: 2050). For example:
   * "1900/12/01", "2019.01.25", "2050-10-30".
   */
  public static final Pattern PATTERN =
      Pattern.compile(
          ("^" + YYYY2050 + "/" + MONTH_NUMBER + "/" + DAY_OF_MONTH + "$")
              + "|"
              + ("^" + YYYY2050 + "-" + MONTH_NUMBER + "-" + DAY_OF_MONTH + "$")
              + "|"
              + ("^" + YYYY2050 + "[.]" + MONTH_NUMBER + "[.]" + DAY_OF_MONTH + "$")
              + "|"
              // The ISO 8601 standard permits "-" separator or no separator.
              + ("^" + YYYY2050 + MONTH_NUMBER + DAY_OF_MONTH + "$"));

  //
  // Required methods
  //

  /**
   * Creates a new IsDateYYYYMMDD.
   *
   * @param ppt the slice with the variable of interest
   */
  private IsDateYYYYMMDD(PptSlice ppt) {
    super(ppt);
  }

  /** Creates a new prototype IsDateYYYYMMDD. */
  private @Prototype IsDateYYYYMMDD() {
    super();
  }

  /** The prototype invariant. */
  private static @Prototype IsDateYYYYMMDD proto = new @Prototype IsDateYYYYMMDD();

  /**
   * Returns the prototype invariant.
   *
   * @return the prototype invariant
   */
  public static @Prototype IsDateYYYYMMDD get_proto() {
    return proto;
  }

  @Override
  public boolean enabled() {
    return dkconfig_enabled;
  }

  @Override
  public IsDateYYYYMMDD instantiate_dyn(@Prototype IsDateYYYYMMDD this, PptSlice slice) {
    return new IsDateYYYYMMDD(slice);
  }

  @SideEffectFree
  @Override
  public String format_using(@GuardSatisfied IsDateYYYYMMDD this, OutputFormat format) {
    if (format != OutputFormat.DAIKON) {
      return format_unimplemented(format);
    }
    return var().name() + " is a Date. Format: YYYY-MM-DD";
  }

  /**
   * Returns true if the given string is a date in the format matched by {@link #PATTERN}, and the
   * date exists in the calendar (for example, it is not February 30).
   *
   * @param v a string
   * @return true if {@code v} is a valid date in this invariant's format
   */
  public static boolean isValid(String v) {
    if (!PATTERN.matcher(v).matches()) {
      return false;
    }
    // The value is either YYYYMMDD (8 characters) or YYYY?MM?DD (10 characters).
    if (v.length() == 8) {
      return isCalendarDate(v.substring(0, 4), v.substring(4, 6), v.substring(6, 8));
    } else {
      return isCalendarDate(v.substring(0, 4), v.substring(5, 7), v.substring(8, 10));
    }
  }

  @Override
  public InvariantStatus check_modified(String v, int count) {
    if (isValid(v)) {
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
    assert other instanceof IsDateYYYYMMDD;
    return true;
  }
}
