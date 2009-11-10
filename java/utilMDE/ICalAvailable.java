package utilMDE;

import net.fortuna.ical4j.model.*;
import net.fortuna.ical4j.model.component.*;
import net.fortuna.ical4j.model.parameter.*;
import net.fortuna.ical4j.model.property.*;
import net.fortuna.ical4j.data.*;

import utilMDE.*;

import java.io.*;
import java.net.URL;
import java.text.*;
import java.util.regex.*;
// Can't "import java.util.*;" because of Date, etc.
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Locale;
import java.util.regex.Pattern;
import java.util.Map;
import java.util.HashMap;


// If you are perplexed because of odd results, maybe it is because of the
// transparency of your iCal items (this shows up as "available/busy" in
// Google calendar).

// TODO:  Fix "Problem:  any all-day events will be treated as UTC." (see below)


/**
 * Given one or more calendars in iCal format, produces a textual summary
 * of available times.  Run
 * <pre>java mde.ICalAvailable --help</pre>
 * to see usage information and options.
 **/
public class ICalAvailable {

  /// User options

  @Option("first date to summarize")
  public static String date = "today";

  public static DateTime start_date = new DateTime();

  @Option("number of calendar days to summarize,")
  public static int days = 8;

  @Option("<url> schedule in iCal format")
  // For a Google calendar:  go to setting, then click on the green "ICAL"
  // icon for the "private addrss".
  public static List<String> iCal_URL = new ArrayList<String>();


  /**
   * A list of time ranges, expressed as a String.
   * Example: 9am-5pm,7:30pm-9:30pm */
  @Option("time ranges during which appointments are permitted")
  public static String business_hours = "9am-5pm";

  static List<Period> businessHours;   // initialize to 9am-5pm
  static List<Integer> businessDays;   // initialize to Mon-Fri

  static TimeZoneRegistry tzRegistry = TimeZoneRegistryFactory.getInstance().createRegistry();
  /**
   * Time zone as an Olson timezone ID, e.g.: America/New_York.
   * Available times are printed in this time zone.  It defaults to the
   * system time zone.
   **/
  // don't need "e.g.: America/New_York" in message:  the default is an example
  @Option("<timezone> time zone")
  public static String timezone1 = TimeZone.getDefault().getID();
  static TimeZone tz1;
  // If I'm outputting in a different timezone, then my notion of a "day"
  // may be different than the other timezone's notion of a "day".  This
  // doesn't seem important enough to fix right now.
  /**
   * Time zone as an Olson timezone ID, e.g.: America/New_York.
   * If set, then free times are printed in two time zones. */
  @Option("<timezone> optional second time zone")
  public static String timezone2;
  static TimeZone tz2;

  /// Other variables

  @Option("enable debugging output")
  public static boolean debug = false;

  /** The appointments (the times that are unavailable for meeting) */
  static List<Calendar> calendars = new ArrayList<Calendar>();

  static DateFormat tf = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.US);
  static DateFormat df = DateFormat.getDateInstance(DateFormat.MEDIUM, Locale.US);
  static DateFormat dffull = DateFormat.getDateInstance(DateFormat.FULL, Locale.US);

  /// Procedures

  @SuppressWarnings("deprecation") // for iCal4j
  static void processOptions(String[] args) {
    Options options = new Options ("ICalAvailable [options]", ICalAvailable.class);
    String[] remaining_args = options.parse_or_usage (args);
    if (remaining_args.length != 0) {
      System.err.println("Unrecognized arguments: " + remaining_args);
      System.exit(1);
    }
    if (iCal_URL.isEmpty()) {
      System.err.println("Option iCal_URL must be specified.");
      System.exit(1);
    }

    // Convert Strings to TimeZones
    tz1 = tzRegistry.getTimeZone(canonicalizeTimezone(timezone1));
    assert tz1 != null;
    if (tz1 == null) {
      throw new Error("didn't find timezone " + timezone1);
    }
    if (timezone2 != null) {
      tz2 = tzRegistry.getTimeZone(canonicalizeTimezone(timezone2));
      if (tz2 == null) {
        System.err.println("Unrecognized time zone (see http://www.php.net/manual/en/timezones.php): " + timezone2);
        System.exit(1);
      }
    }

    try {
      if (! date.equals("today")) {
        start_date = new DateTime(parseDate(date));
      }
    } catch (Exception e) {
      if (Pattern.matches(".*/.*", date)
          && ! Pattern.matches(".*/.*/", date)) {
        System.err.println("Could not parse date (missing year?): " + date);
        System.exit(1);
      } else {
        System.err.println("Could not parse date: " + date);
        System.exit(1);
      }
    }
    if (start_date == null) {
      System.err.println("Could not parse date: " + date);
      System.exit(1);
    }

    // Problem:  this may change the actual time?
    start_date.setTimeZone(tz1);
    start_date.setMinutes((start_date.getMinutes() / 15) * 15);

    for (String URL : iCal_URL) {
      try {
        URL url = new URL(URL);
        CalendarBuilder builder = new CalendarBuilder();
        Calendar c = builder.build(url.openStream());
        calendars.add(c);
      } catch (Exception e) {
        e.printStackTrace(System.err);
        System.err.println("Could not read calendar from " + URL);
        System.exit(1);
      }
    }

    businessHours = new ArrayList<Period>();
    for (String range : business_hours.split(",")) {
      String[] startEnd = range.split("-");
      if (startEnd.length != 2) {
        System.err.println("Bad time range: " + range);
        System.exit(1);
      }
      DateTime busStart = parseTime(startEnd[0]);
      DateTime busEnd = parseTime(startEnd[1]);
      businessHours.add(new Period(busStart, busEnd));
    }

    businessDays = new ArrayList<Integer>();
    businessDays.add(1);
    businessDays.add(2);
    businessDays.add(3);
    businessDays.add(4);
    businessDays.add(5);

  }

  static Map<String,String> canonicalTimezones = new HashMap<String,String>();
  static Map<String,String> printedTimezones = new HashMap<String,String>();
  // Yuck, this should really be a separate configuration file.
  static {
    canonicalTimezones.put("eastern", "America/New_York");
    canonicalTimezones.put("est", "America/New_York");
    canonicalTimezones.put("edt", "America/New_York");
    canonicalTimezones.put("boston", "America/New_York");
    canonicalTimezones.put("america/boston", "America/New_York");
    canonicalTimezones.put("central", "America/Chicago");
    canonicalTimezones.put("pacific", "America/Los_Angeles");
    canonicalTimezones.put("pst", "America/Los_Angeles");
    canonicalTimezones.put("pacific standard time", "America/Los_Angeles");
    canonicalTimezones.put("pdt", "America/Los_Angeles");
    canonicalTimezones.put("india", "Asia/Calcutta");
    canonicalTimezones.put("china", "Asia/Shanghai");
    canonicalTimezones.put("berlin", "Europe/Berlin");
    canonicalTimezones.put("israel", "Asia/Tel_Aviv");

    printedTimezones.put("Eastern Standard Time", "Eastern");
    printedTimezones.put("Central Standard Time", "Central");
    printedTimezones.put("Pacific Standard Time", "Pacific");
  }


  static String canonicalizeTimezone(String timezone) {
    String result = canonicalTimezones.get(timezone.toLowerCase());
    return (result == null) ? timezone : result;
  }

  static String printedTimezone(TimeZone tz) {
    String tzString = tz.getDisplayName();
    String result = printedTimezones.get(tzString);
    return (result == null) ? tzString : result;
  }

  static Pattern timeRegexp = Pattern.compile("([0-2]?[0-9])(:([0-5][0-9]))?([aApP][mM])?");

  // Parse a time like "9:30pm"
  @SuppressWarnings("deprecation") // for iCal4j
  static DateTime parseTime(String time) {

    Matcher m = timeRegexp.matcher(time);
    if (! m.matches()) {
      System.err.println("Bad time: " + time);
      System.exit(1);
    }
    @SuppressWarnings("nullness")
    /*@NonNull*/ String hourString = m.group(1);
    String minuteString = m.group(3);
    String ampmString = m.group(4);

    int hour = Integer.parseInt(hourString);
    if ((ampmString != null) && ampmString.toLowerCase().equals("pm")) {
      hour += 12;
    }
    int minute = 0;
    if (minuteString != null) {
      minute = Integer.parseInt(minuteString);
    }

    DateTime result = new DateTime();
    result.setTimeZone(tz1);
    result.setHours(hour);
    result.setMinutes(minute);
    result.setSeconds(0);

    return result;
  }


  // For debugging
  static void printOptions() {
    System.out.println("business_hours: " + business_hours);
    System.out.println("businessHours: " + businessHours);
    System.out.println("businessDays: " + businessDays);
    System.out.println("timezone1: " + timezone1);
    System.out.println("timezone2: " + timezone2);
    System.out.println("start_date: " + start_date);
    System.out.println("days: " + days);
    System.out.println("iCal_URL: " + iCal_URL);
  }

  public static void main(String[] args) {

    processOptions(args);

    List<Period> available = new ArrayList<Period>();
    if (debug) {
      System.err.printf("Testing %d days%n", days);
    }
    for (int i = 0; i<days; i++) {
      available.addAll(oneDayAvailable(start_date, calendars));
      start_date = new DateTime(start_date.getTime() + 1000 * 60 * 60 * 24);
      start_date.setTimeZone(tz1);
    }

    if (tz2 != null) {
      System.out.printf("Timezone: %s  [Timezone: %s]%n",
                        printedTimezone(tz1), printedTimezone(tz2));
    }
    String lastDateString = null;
    for (Period p : available) {
      String dateString = formatDate(p.getStart(), tz1);
      if (! dateString.equals(lastDateString)) {
        lastDateString = dateString;
        System.out.println();
        System.out.println(dateString + ":");
      }

      String rangeString = rangeString(p, tz1);

      if (tz2 == null) {
        System.out.println(rangeString);
      } else {
        String rangeString2 = rangeString(p, tz2);
        System.out.printf("%-20s[%s]%n", rangeString, rangeString2);
      }
    }

  }

  static String rangeString(Period p, TimeZone tz) {
    tf.setTimeZone(tz);
    DateTime pstart = p.getStart();
    DateTime pend = p.getEnd();
    String rangeString = tf.format(pstart) + " to " + tf.format(pend);
    rangeString = rangeString.replace(" AM", "am");
    rangeString = rangeString.replace(" PM", "pm");
    return rangeString;
  }

  static String periodListString(PeriodList pl, TimeZone tz) {
    tf.setTimeZone(tz);
    StringBuilder result = new StringBuilder();
    for (Object p : pl) {
      assert p != null : "@SuppressWarnings(nullness)";
      result.append(rangeString((/*@NonNull*/ Period)p, tz) + "\n");
    }
    return result.toString();
  }


  /**
   * Creates a new DateTime with date taken from the first argument and
   * time taken from the second argument.
   **/
  @SuppressWarnings("deprecation") // for iCal4j
  static DateTime mergeDateAndTime(DateTime date, DateTime time) {
    if (date.getTimeZone() != time.getTimeZone()) {
      throw new Error(String.format("non-matching timezones: %s %s", date.getTimeZone(), time.getTimeZone()));
    }
    DateTime result = new DateTime(date);
    result.setHours(time.getHours());
    result.setMinutes(time.getMinutes());
    result.setSeconds(time.getSeconds());
    return result;
  }


  // TODO:  don't propose times that are before the current moment.

  // Process day-by-day because otherwise weekends and evenings are included.
  @SuppressWarnings("unchecked") // for iCal4j
  static List<Period> oneDayAvailable(DateTime day, List<Calendar> calendars) {
    if (debug) {
      System.err.printf("oneDayAvailable(%s, ...)%n", day);
    }
    List<Period> result = new ArrayList<Period>();
    @SuppressWarnings("deprecation") // for iCal4j
    int dayOfWeek = day.getDay();
    if (! businessDays.contains(dayOfWeek)) {
      return result;
    }

    for (Period bh : businessHours) {
      DateTime start = mergeDateAndTime(day, bh.getStart());
      DateTime end = mergeDateAndTime(day, bh.getEnd());

      VFreeBusy request = new VFreeBusy(start, end, new Dur(0, 0, 0, 1));
      if (debug) {
        System.out.println("Request = " + request);
      }
      ComponentList busyTimes = new ComponentList();
      // Problem:  any all-day events will be treated as UTC.
      // Instead, they should be converted to local time (tz1).
      // But VFreeBusy does not support this, so I may need to convert
      // daily events into a different format before inserting them.
      for (Calendar calendar : calendars) {
        for (Iterator<Object> itor = calendar.getComponents().iterator(); itor.hasNext(); ) {
          Component c = (Component) itor.next();
          if (c instanceof VEvent) {
            VEvent v = (VEvent) c;
            DtStart dts = v.getStartDate();
            Parameter dtsValue = dts.getParameter("VALUE");
            boolean allDay = (dtsValue != null) && dtsValue.getValue().equals("DATE");
            // TODO: convert to the proper timezone.
            // Tricky: must deal with the possibility of RRULE:FREQ=
          }
          busyTimes.add(c);
        }
      }
      VFreeBusy response = new VFreeBusy(request, busyTimes);
      if (debug) {
        System.out.println("Response = " + response);
      }
      FreeBusy freefb = (FreeBusy) response.getProperty("FREEBUSY");
      if (freefb == null) {
        if (debug) {
          System.out.println("FREEBUSY property is null");
        }
        continue;
      }
      assert freefb.getParameter(Parameter.FBTYPE) == FbType.FREE;
      PeriodList freePeriods = freefb.getPeriods();
      if (debug) {
        System.out.printf("Free periods: %n%s%n", periodListString(freePeriods, tz1));
      }
      result.addAll(freePeriods);
    }
    if (debug) {
      System.err.printf("oneDayAvailable(%s, ...) => %s elements%n", day, result.size());
    }
    return result;
  }

  static SimpleDateFormat[] dateFormats
    = { new SimpleDateFormat( "yyyy/MM/dd" ),
        new SimpleDateFormat( "MM/dd/yyyy" ),
        new SimpleDateFormat( "MM/dd/yy" ),
        // Bad idea:  sets year to 1970.  So require the year, at least for now.
        // new SimpleDateFormat( "MM/dd" ),
  };

  /**
   * Parses a date when formatted in several common formats.
   * @see dateFormats
   **/
  static java.util.Date parseDate( String strDate ) throws ParseException {
    java.util.Date result = null;
    if (Pattern.matches("^[0-9][0-9]?/[0-9][0-9]?$", date)) {
      @SuppressWarnings("deprecation") // for iCal4j
      int year = new Date().getYear() + 1900;
      strDate = strDate + "/" + year;
    }
    for (DateFormat this_df : dateFormats) {
      this_df.setLenient(false);
      try {
        result = this_df.parse( strDate );
        return result;
      } catch ( ParseException e ) {
        // Try the next format in the list.
      }
    }
    throw new ParseException("bad date " + strDate, 0);
  }

  static String formatDate(DateTime d, TimeZone tz) {
    df.setTimeZone(tz);
    String result = df.format(d);
    // Don't remove trailing year; it's a good double-check.
    // Remove trailing year, such as ", 1952".
    // result = result.substring(0, result.length() - 6);
    // Prepend day of week.
    result = dffull.format(d).substring(0,3) + " " + result;
    return result;
  }

}
