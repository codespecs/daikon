package daikon;

import org.apache.log4j.Logger;
import org.apache.log4j.Level;
import org.apache.log4j.Layout;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Appender;
import org.apache.log4j.FileAppender;

import java.io.*;

/**
 * Standard methods for setting up logging with Log4j.
 * Allows creation of Console writers using one method.
 * Logger methods should only be called in a shell class
 * at setup, after which Logger calls should be used
 * for logging.
 **/
public class LogHelper {

  // Class variables so user doesn't have to use log4j.Level
  public static final Level DEBUG = Level.DEBUG;
  public static final Level INFO = Level.INFO;
  public static final Level WARN = Level.WARN;
  public static final Level ERROR = Level.ERROR;
  public static final Level FATAL = Level.FATAL;

  /**
   * Sets up global logs with a given priority and logging output
   * pattern.  Creates one ConsoleAppender at root to receive default
   * messages, setting priority to INFO.  Removes previous appenders
   * at root.
   **/
  public static void setupLogs(Level l, String pattern) {
    // Example: "@daikon.Daikon: This is a message \n"
    Layout layout = new PatternLayout(pattern);
    // Send debug and other info messages to System.err
    Appender app = new ConsoleAppender (layout, ConsoleAppender.SYSTEM_ERR);

    Logger.getRoot().removeAllAppenders();
    Logger.getRoot().addAppender(app);
    Logger.getRoot().setLevel(l);
    Logger.getRoot().debug("Installed logger at level " + l);
  }

  /**
   * Default method for setting up global logs.
   **/
  public static void setupLogs() {
    setupLogs (INFO);
  }

  /**
   * Sets up global logs with a given priority.
   * Creates one ConsoleAppender.  Removes previous appenders at root.
   **/
  public static void setupLogs(Level l) {
    setupLogs (l, "@ %20.20c: %m%n");
    // By default, take up 20 spaces min, and 20 spaces max for
    // %c = Logger. %m = message, %n = newline
  }

  /**
   * Changes the logging priority of a sub category
   **/
  public static void setLevel(String s, Level l) {
    Logger.getLogger(s).setLevel(l);
  }

}
