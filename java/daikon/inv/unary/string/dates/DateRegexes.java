package daikon.inv.unary.string.dates;

import java.time.YearMonth;
import org.checkerframework.checker.regex.qual.Regex;

/** Regular expressions, and related helper methods, used by date invariants. */
public class DateRegexes {

  /** Do not instantiate. */
  private DateRegexes() {
    throw new Error("Do not instantiate.");
  }

  /** A regex for a 4-digit year. The regex is unanchored and ungrouped. */
  public static final @Regex String YYYY = "\\d{4}";

  /**
   * A regex for a 4-digit year, between 1900 and 2050 inclusive. The regex is unanchored and
   * ungrouped.
   */
  public static final @Regex String YYYY2050 = "(?:19\\d{2}|20[01234][0-9]|2050)";

  /** A regex for a 2-digit month. The regex is unanchored and ungrouped */
  public static final @Regex String MONTH_NUMBER = "(?:0[1-9]|1[012])";

  /** A regex for a 2-digit day of the month. The regex is unanchored and ungrouped */
  public static final @Regex String DAY_OF_MONTH = "(?:0[1-9]|[12][0-9]|3[01])";

  /** A regex for a 2-digit hour in 24-hour format. The regex is unanchored and ungrouped. */
  public static final @Regex String HH = "(?:[01][0-9]|2[0-3])";

  /**
   * A regex for a 1-digit or 2-digit hour in 24-hour format. The regex is unanchored and ungrouped.
   */
  public static final @Regex String H = "(?:0?[0-9]|1[0-9]|2[0-3])";

  /**
   * A regex for a 1-digit or 2-digit hour in 12-hour format. The regex is unanchored and ungrouped.
   */
  public static final @Regex String H12 = "(?:0?[1-9]|1[0-2])";

  /** A regex for a 2-digit minutes. The regex is unanchored and ungrouped. */
  public static final @Regex String MINUTES = "[0-5][0-9]";

  /** A regex for a 2-digit seconds. The regex is unanchored and ungrouped. */
  public static final @Regex String SECONDS = "[0-5][0-9]";

  /**
   * A regex for a 3-digit milliseconds, starting with a period. The regex is unanchored and
   * ungrouped.
   */
  public static final @Regex String mmm = "\\.[0-9]{3}";

  /**
   * A regex for an optional 3-digit milliseconds, starting with a period. The regex is unanchored
   * and ungrouped.
   */
  public static final @Regex String mmmOptional = "(?:" + mmm + ")?";

  /**
   * A regex for meridiems (AM/PM), with optional leading space. The regex is unanchored and
   * ungrouped.
   */
  public static final @Regex String AMPM = " ?([AaPp][Mm])";

  /**
   * Returns true if the given year, month, and day form a date that exists in the calendar. For
   * example, returns false for February 30, and for February 29 in a non-leap year. Each argument
   * must consist of decimal digits; the month must be between 1 and 12 and the day between 1 and
   * 31, as guaranteed by {@link #MONTH_NUMBER} and {@link #DAY_OF_MONTH}.
   *
   * @param year the year, as a string of digits
   * @param month the month number, as a string of digits
   * @param day the day of the month, as a string of digits
   * @return true if the date exists in the calendar
   */
  public static boolean isCalendarDate(String year, String month, String day) {
    return YearMonth.of(Integer.parseInt(year), Integer.parseInt(month))
        .isValidDay(Integer.parseInt(day));
  }
}
