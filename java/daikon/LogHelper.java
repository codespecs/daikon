package daikon;

import org.apache.log4j.Logger;
import org.apache.log4j.Priority;
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

    Logger.getRootLogger().removeAllAppenders();
    Logger.getRootLogger().addAppender (app);
    Logger.getRootLogger().setPriority(p);
    Logger.getRootLogger().debug ("Installed logger at priority " + p);

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
    // By default, take up 20 spaces min, and 20 spaces max for
    // %c = Logger. %m = message, %n = newline
  }

  /**
   * Changes the logging priority of a sub category
   **/
  public static void setPriority (String s, Priority p) {
    Logger.getLogger(s).setPriority(p);
    if (s.equals (PrintInvariants.debugFiltering.getName())
        && PrintInvariants.debugFiltering.isDebugEnabled()) {
      // Set up filter logging to go to a file.  It's better that way.
      PrintInvariants.debugFiltering.setAdditivity(false);
      PrintInvariants.debugFiltering.removeAllAppenders();
      FileAppender fa = null;
      try {
        fa = new FileAppender( new PatternLayout("%m"),
                               PrintInvariants.daikonFilteringOutputFilename, true);
        fa.setName (PrintInvariants.daikonFilteringOutputFilename);
      } catch (IOException ioe) {
        System.err.println("Error; unable to open file " +
                           PrintInvariants.daikonFilteringOutputFilename +
                           " for debuging filtering");
        throw new Error();
      }
      if (fa != null) {
        PrintInvariants.debugFiltering.addAppender(fa);
        fa.activateOptions();
      } else {
        System.err.println("Error; unable to open file " +
                           PrintInvariants.daikonFilteringOutputFilename +
                           " for debuging filtering");
        throw new Error();
      }
    }
  }

}
