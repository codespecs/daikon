package daikon.split;

import java.lang.*;
import java.io.*;
import java.util.*;

/**
 * This class can be used to keep time limits on a subprocess being
 * executed by the Java Runtime. This class is represented by a
 * process and the commandline used to create the process. The
 * waitFor(long waitTime) method re-executes the command-line for a
 * fixed length of time, after which it terminates the process if it's
 * still running.
 */

public class TimedProcess {

  Timer timer;
  String command;
  Process p;
  boolean finished; //keep track of whether the process if finished or not.
  long waitTime;
  Runtime commander = java.lang.Runtime.getRuntime();
  BufferedReader error;
  StringBuffer errorMessage;

  //dkconfig_ variables should only be set via the configuration
  //options

  /**
   * Positive integer.  Specifies the Splitter compilation timeout, in
   * seconds, after which the compilation process is terminated and
   * retried, on the assumption that it has hung.
   **/
  public static int dkconfig_compile_timeout = 6;

  /**
   * @requires: p != null && command != null
   */
  public TimedProcess (Process p, String command) {
    this.p = p;
    this.command = command;
    error = new BufferedReader(new InputStreamReader(p.getErrorStream()));
    errorMessage = new StringBuffer();
  }

  /**
   * @return the error message from execution of this process
   */
  public String getErrorMessage () {
    String s;
    try {
      while ((s = error.readLine()) != null) {
        errorMessage.append(s);
      }
      return errorMessage.toString();
    } catch (IOException ioe) {
      System.out.println("TimedProcess: " + ioe.toString());
    }
    return errorMessage.toString();
  }

  /**
   * @return true if the process if finished, false otherwise
   */
  public boolean finished () {
    try {
      //sad, but the call to Process.exitValue() causes it to throw
      //an exception if the process is not finished
      String s;
      while ((s = error.readLine()) != null) {
        errorMessage.append(s);
      }
      int exit =  p.exitValue();
      return true;
    } catch (IllegalThreadStateException ie) {
      //do nothing.
    } catch (IOException ioe) {
      System.out.println("TimedProcess: " + ioe.toString());
    }
    return false;
  }

  /**
   * The command used to create this process is re-executed for
   * <dkconfig_compile_timeout> (in seconds). This value is set in the
   * configuration settings. The resulting process is terminated after
   * that time, if it's not done */

  public void waitFor( ) {
    waitTime = dkconfig_compile_timeout;
    timer = new Timer(true);
    try {
      timer.schedule(new timerTask(), dkconfig_compile_timeout*1000);
      p = commander.exec(command);
      p.waitFor();
    } catch (IOException ie) {
      System.out.println("TimedProcess: " + ie.toString() + " while re-executing command " + command);
    } catch (InterruptedException ie) {
      System.out.println("TimedProcess: " + ie.toString() + " while re-executing command " + command);
    }
  }

  /**
   * At the scheduled time, this TimerTask destroys the process
   * represented by this TimedProcess
   */
  class timerTask extends TimerTask {
    public void run() {
      try {
        int exit = p.exitValue();
        //System.out.println("\nProcess " + command + "\nexited with status " + exit);
      } catch (IllegalThreadStateException ie) {
        System.out.println("Process " + command + "\nterminated after "
                           + waitTime + " seconds");
      }
      p.destroy();
      timer.cancel();
    }
  }
}
