package daikon;

import java.util.logging.*;

/**
 * Standard methods for setting up logging with Log4j.
 * Allows creation of Console writers using one method.
 * Logger methods should only be called in a shell class
 * at setup, after which Logger calls should be used
 * for logging.
 **/
public class LogHelper {

  // Class variables so user doesn't have to use log4j.Level
  public static final Level FINE = Level.FINE;
  public static final Level INFO = Level.INFO;
  public static final Level WARNING = Level.WARNING;
  public static final Level SEVERE = Level.SEVERE;

  /**
   * Sets up global logs with a given priority and logging output
   * pattern.  Creates one ConsoleHandler at root to receive default
   * messages, setting priority to INFO.  Removes previous appenders
   * at root.
   **/
  public static void setupLogs(Level l, Formatter formatter) {
    // Send debug and other info messages to System.err
    Handler app = new ConsoleHandler();
    app.setFormatter(formatter);

    // Logger.global.removeAllAppenders();
    {
      Handler[] handlers = Logger.global.getHandlers();
      for (int i=0; i<handlers.length; i++) {
        Logger.global.removeHandler(handlers[i]);
      }
    }

    Logger.global.addHandler(app);
    Logger.global.setLevel(l);
    Logger.global.fine ("Installed logger at level " + l);
  }

  // Statically initialized to save runtime
  private static String padding_arrays[] = new String[] {
    "",
    " ",
    "  ",
    "   ",
    "    ",
    "     ",
    "      ",
    "       ",
    "        ",
    "         ",
    "          ",
    "           ",
    "            ",
    "             ",
    "              ",
    "               ",
    "                ",
    "                 ",
    "                  ",
    "                   ",
  };

  public static class DaikonLogFormatter extends SimpleFormatter {
    public String format(LogRecord record) {
      // // By default, take up 20 spaces min, and 20 spaces max for
      // // %c = Logger. %m = message, %n = newline
      // // Example: "@daikon.Daikon: This is a message \n"
      // setupLogs (l, "@ %20.20c: %m%n");

      String loggerName = record.getLoggerName();
      int loggerNameLength = loggerName.length();
      if (loggerNameLength > 20) {
        loggerName = loggerName.substring(0, 20);
      } else if (loggerNameLength < 20) {
        loggerName = loggerName + padding_arrays[20 - loggerNameLength];
      }
      return loggerName + record.getMessage();
    }
  }


  /**
   * Default method for setting up global logs.
   **/
  public static void setupLogs() {
    setupLogs (INFO);
  }

  /**
   * Sets up global logs with a given priority.
   * Creates one ConsoleHandler.  Removes previous appenders at root.
   **/
  public static void setupLogs(Level l) {
    setupLogs (l, new DaikonLogFormatter());
  }

  /**
   * Changes the logging priority of a sub category
   **/
  public static void setLevel(String s, Level l) {
    Logger.getLogger(s).setLevel(l);
  }

}
