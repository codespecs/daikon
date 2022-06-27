package daikon;

import static java.util.logging.Level.INFO;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

/**
 * Standard methods for setting up logging. Allows creation of Console writers using one method.
 * Logger methods should only be called in a shell class at setup, after which Logger calls should
 * be used for logging.
 */
public final class LogHelper {
  private LogHelper() {
    throw new Error("do not instantiate");
  }

  /**
   * Sets up global logs with a given priority and logging output pattern. Creates one
   * ConsoleHandler at root to receive default messages, setting priority to INFO. Removes previous
   * appenders at root.
   */
  public static void setupLogs(Level l, Formatter formatter) {
    // Send debug and other info messages to System.err
    Handler app = new ConsoleHandler();
    app.setLevel(Level.ALL);
    app.setFormatter(formatter);

    // Logger.global.removeAllAppenders();
    {
      // Java 5 version
      @SuppressWarnings("deprecation")
      Logger global = Logger.global;
      // Java 6 version (doesn't work in Java 5)
      // Logger global = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
      Handler[] handlers = global.getHandlers();
      for (Handler handler : handlers) {
        global.removeHandler(handler);
      }
    }

    Logger root = Logger.getLogger("");
    Handler[] handlers = root.getHandlers();
    for (Handler handler : handlers) {
      root.removeHandler(handler);
    }
    root.addHandler(app);
    root.setLevel(l);
    allLoggers.add(root);

    // Logger.global.addHandler(app);
    // Logger.global.setLevel(l);
    // Logger.global.fine ("Installed logger at level " + l);
  }

  public static class DaikonLogFormatter extends SimpleFormatter {
    @Override
    public synchronized String format(LogRecord record) {
      // // By default, take up 20 spaces min, and 20 spaces max for logger.
      // // %c = Logger. %m = message, %n = newline
      // // Example output: "@ daikon.Daikon: This is a message"
      // setupLogs (l, "@ %20.20c: %m%n");

      String loggerName = record.getLoggerName() + ":";

      // If we aren't generating tracebacks, find the src class/method/line
      // where the log was called from
      String src = "";
      if (!Debug.dkconfig_showTraceback) {
        Throwable stack = new Throwable("debug traceback");
        stack.fillInStackTrace();
        StackTraceElement[] ste_arr = stack.getStackTrace();
        for (int ii = ste_arr.length - 1; ii >= 0; ii--) {
          StackTraceElement ste = ste_arr[ii];
          if (ste.getClassName().startsWith("java")
              || ste.getClassName().contains("daikon.LogHelper")
              || ste.getMethodName().equals("log")
              || ste.getClassName().contains("daikon.Debug")) {
            continue;
          }
          src = ste.getFileName() + " " + ste.getLineNumber() + ": ";
        }
      }
      // src = record.getSourceClassName().replaceAll ("\\w*\\.", "")
      //        + "." + record.getSourceMethodName() + ": ";

      return "@ " + loggerName + " " + src + record.getMessage() + Global.lineSep;
    }
  }

  /** Default method for setting up global logs. */
  public static void setupLogs() {
    setupLogs(INFO);
  }

  /**
   * Sets up global logs with a given priority. Creates one ConsoleHandler. Removes previous
   * appenders at root.
   */
  public static void setupLogs(Level l) {
    setupLogs(l, new DaikonLogFormatter());
  }

  private static final Set<Logger> allLoggers = new HashSet<>();

  /**
   * Changes the logging priority of a sub category. Also caches the logger to avoid
   * garbage-collection and recreation with the old level.
   */
  public static void setLevel(Logger lg, Level l) {
    lg.setLevel(l);
    allLoggers.add(lg); // to prevent garbage-collection and re-initialization
  }

  /**
   * Changes the logging priority of a sub category. Also caches the logger to avoid
   * garbage-collection and recreation with the old level.
   */
  public static void setLevel(String s, Level l) {
    setLevel(Logger.getLogger(s), l);
  }
}
