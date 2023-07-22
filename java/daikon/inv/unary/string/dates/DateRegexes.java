package daikon.inv.unary.string.dates;

import org.checkerframework.checker.regex.qual.Regex;

/** Regular expressions used by date invariants. */
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

  /** A regex for a 2-digit month. The regex is unanchored and ungrouped */
  public static final @Regex String DAY_OF_MONTH = "(?:[0-2][0-9]|3[01])";

  /** A regex for a 2-digit hour in 24-hour format. The regex is unanchored and ungrouped. */
  public static final @Regex String HH = "(?:[0-9]|1[0-9]|2[0-3])";

  /**
   * A regex for a 1-digit or 2-digit hour in 24-hour format. The regex is unanchored and ungrouped.
   */
  public static final @Regex String H = "(?:0?[0-9]|1[0-9]|2[0-3])";

  /** A regex for a 2-digit minutes. The regex is unanchored and ungrouped. */
  public static final @Regex String MINUTES = "[0-5][0-9]";

  /** A regex for a 2-digit seconds. The regex is unanchored and ungrouped. */
  public static final @Regex String SECONDS = "[0-5][0-9]";

  /** A regex for a 3-digit milliseconds. The regex is unanchored and ungrouped. */
  public static final @Regex String mmm = "\\.[0-9]{3}";

  /** A regex for optional 3-digit milliseconds. The regex is unanchored and ungrouped. */
  public static final @Regex String mmmOptional = "(?:" + mmm + ")?";

  /**
   * A regex for meridiens (AM/PM), with optional leading space. The regex is unanchored and
   * ungrouped.
   */
  public static final @Regex String AMPM = " ?(?:0[1-9]|1[012])";
}
