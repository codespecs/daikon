package daikon;

import org.apache.log4j.Category;
import org.apache.log4j.Priority;
import org.apache.log4j.Layout;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Appender;

/**
 * Standard methods for setting up logging with Log4j.
 * Allows creation of Console writers using one method.
 * Logger methods should only be called in a shell class
 * at setup, after which Category calls should be used
 * for logging. 
 *
 **/

public class Logger {

  // Class variables so user doesn't have to use log4j.Priority

  public static final Priority DEBUG = Priority.DEBUG;
  public static final Priority INFO = Priority.INFO;
  public static final Priority WARN = Priority.WARN;
  public static final Priority ERROR = Priority.ERROR;
  public static final Priority FATAL = Priority.FATAL;

  /**
   * Sets up global logs with a given priority and logging output
   * pattern.  Creates one ConsoleAppender at root to receive default
   * messages, setting priority to INFO.  Removes previous appenders
   * at root.
   **/

  public static void setupLogs(Priority p, String pattern) {

    // Example: "@daikon.Daikon: This is a message \n"
    Layout layout = new PatternLayout(pattern);
    // Send debug and other info messages to System.err
    Appender app = new ConsoleAppender (layout, ConsoleAppender.SYSTEM_ERR);

    Category.getRoot().removeAllAppenders();
    Category.getRoot().addAppender (app);
    Category.getRoot().setPriority(p);
    Category.getRoot().debug ("Installed logger at priority " + p);

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

  public static void setupLogs(Priority p) {
    setupLogs (p, "@ %20.20c: %m%n");
  }

  /**
   * Changes the logging priority of a sub category
   *
   **/

  public static void setPriority (String s, Priority p) {
    Category.getInstance(s).setPriority(p);
  }


}
