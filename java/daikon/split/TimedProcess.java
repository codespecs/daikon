package daikon.split;

import java.lang.*;
import java.io.*;
import java.util.*;

/**
 * This class can be used to keep time limits on a subprocess being
 * executed by the Java Runtime. The waitFor(long waitTime) command
 * re-executes the command-line for a fixed length of time,
 * after which it terminates the process if it's still running.
 */

public class TimedProcess {

  Timer timer;
  String command;
  Process p;
  boolean finished; //keep track of whether the process if finished or not.
  long waitTime;
  Runtime commander = Runtime.getRuntime();

  /**
   * @requires: p != null && command != null
   */
  public TimedProcess (Process p, String command) {
    this.p = p;
    this.command = command;
  }

  /**
   * @returns: true if the process if finished, false otherwise.
   *           If false, the process is terminated
   */
  public boolean finished () {
    try {
      if ( p.exitValue() == 0 ) {
	return true;
      } else {
	return false;
      }
    } catch (IllegalThreadStateException ie) {
      return false;
    }
  }

  /**
   * @ensures: the commandline is re-executed for <seconds> (the time,
   *           in seconds) The resulting process is terminated after
   *           that time, if it's not done
   */

  public void waitFor(long seconds) {
    waitTime = seconds;
    timer = new Timer(true);
    try {
      timer.schedule(new timerTask(), seconds*1000);
      p = commander.exec(command);
      System.out.println("Waiting " + waitTime + "s for process " + command  + " to complete ....");
      p.waitFor();
    } catch (IOException ie) {
      System.out.println("TimedProcess: " + ie.toString() + " while re-executing command " + command);
    } catch (InterruptedException ie) {
      System.out.println("TimedProcess: " + ie.toString() + " while re-executing command " + command);
    }
  }

  /**
   *@ ensures: destroys the process after the fixed amount of time, if it's not done
   */
  class timerTask extends TimerTask {
    public void run() {
      try {
	if ( p.exitValue() != 0 ) {
	  System.out.println("Execution of command \n" + command + "\n taking > "
			     + waitTime + " seconds.... Process terminated\n");
	}
      } catch (IllegalThreadStateException ie) {
	System.out.println("TimedProcess" + ie.toString());
      }
      p.destroy();
      timer.cancel();
    }
  }
}
