package utilMDE;

import java.util.Stack;
import java.io.PrintStream;

public final class SimpleLog {

  public String indent_str = "";
  public boolean enabled;
  public boolean line_oriented = true;
  public boolean always_tb = false;

  public PrintStream logfile = System.out;

  public static class LongVal {
    public long val;
    public LongVal (long val) {
      this.val = val;
    }
  }

  public Stack<LongVal> start_times = new Stack<LongVal>();

  public SimpleLog (boolean enabled, boolean always_tb) {
    this.enabled = enabled;
    this.always_tb = always_tb;
    start_times.push (new LongVal(System.currentTimeMillis()));
  }

  public SimpleLog (boolean enabled) {
    this (enabled, false);
  }

  public SimpleLog() {
    this (true);
  }

  public SimpleLog (String filename, boolean enabled) {
    this (enabled);
    try {
      logfile = new PrintStream (filename);
    } catch (Exception e) {
      throw new RuntimeException ("Can't open " + filename, e);
    }
  }

  public final boolean enabled() {
    return enabled;
  }

  public final void indent() {
    if (enabled) {
      indent_str += "  ";
      start_times.push (new LongVal(System.currentTimeMillis()));
    }
  }

  public final void indent (String format, Object... args) {
    if (enabled) {
      log (format, args);
      indent();
    }
  }

  public final void exdent() {
    if (enabled) {
      indent_str = indent_str.substring (0, indent_str.length()-2);
      start_times.pop();
    }
  }

  public final void exdent (String format, Object... args) {
    if (enabled) {
      exdent();
      log (format, args);
    }
  }

  public final void exdent_time (String format, Object... args) {
    if (enabled) {
      exdent();
      log_time (format, args);
    }
  }

  public final void log (String format, Object... args) {

    if (enabled) {
      format = fix_format(format);
      logfile.print (indent_str);
      logfile.printf (format, args);
      if (always_tb)
        tb();
    }

  }

  public final void log_tb (String format, Object... args) {
    if (enabled) {
      log (format, args);
      tb();
    }
  }

  public final void tb() {
    Throwable t = new Throwable();
    t.fillInStackTrace();
    StackTraceElement[] ste_arr = t.getStackTrace();
    for (int ii = 2; ii < ste_arr.length; ii++) {
      StackTraceElement ste = ste_arr[ii];
      logfile.printf ("%s  %s%n", indent_str, ste);
    }
  }

  private final String fix_format (String format) {

    if (!line_oriented)
      return format;

    if (format.endsWith ("%n"))
      return format;

    return format + "%n";
  }

  public final void start_time() {
    if (enabled)
      start_times.peek().val = System.currentTimeMillis();
  }

  /**
   * Writes the specified message and the elapsed time since
   * the last call to start_time().  Calls start_time() after
   * printing message
   */
  public final void log_time (String format, Object... args) {

    if (enabled) {
      long elapsed = System.currentTimeMillis() - start_times.peek().val;
      logfile.print (indent_str);
      if (elapsed > 1000)
        logfile.printf ("[%,f secs] ", elapsed/1000.0);
      else
        logfile.print ("[" + elapsed + " ms] ");
      format = fix_format(format);
      logfile.printf (format, args);
      // start_time();
    }
  }
}
