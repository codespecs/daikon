package utilMDE;

import java.io.*;
import java.util.*;

/**
 * TimeLimitProcess is a subclass of Process such that the process is
 * killed if it runs for more than the specified number of milliseconds.
 * Wall clock seconds, not CPU seconds, are measured.
 **/

public class TimeLimitProcess {

  private Process p;
  private long timeLimit;

  private Timer timer;

  private BufferedReader errorStream;
  private StringBuffer errorMessage;

  /**
   * Requires: p != null && command != null
   * @param timeLimit in milliseconds
   **/
  public TimeLimitProcess (Process p, long timeLimit) {
    this.p = p;
    timer = new Timer(true);
    timer.schedule(new TPTimerTask(p), timeLimit);
  }

  /**
   * Kills the subprocess.
   * @see Process.destroy()
   **/
  void destroy() {
    p.destroy();
  }

  /**
   * Returns the exit value for the subprocess.
   * @see Process.getErrorStream()
   */
  int exitValue() {
    return p.exitValue();
  }

  /**
   * Gets the error stream of the subprocess.
   * @see Process.getErrorStream()
   */
  InputStream getErrorStream() {
    return p.getErrorStream();
  }

  /**
   * Gets the input stream of the subprocess.
   * @see Process.getInputStream()
   */
  InputStream getInputStream() {
    return p.getInputStream();
  }

  /**
   * Gets the output stream of the subprocess.
   * @see Process.getOutputStream()
   */
  OutputStream getOutputStream() {
    return p.getOutputStream();
  }

  /**
   * Causes the current thread to wait, if necessary, until the process represented by this Process object has terminated.
   * @see Process.waitFor()
   */
  int waitFor() throws InterruptedException {
    return p.waitFor();
  }

  /**
   * @return true if the process if finished, false otherwise
   **/
  public boolean finished () {
    try {
      // Process.exitValue() throws an exception if the process is not
      // finished.
      p.exitValue();
      return true;
    } catch (IllegalThreadStateException ie) {
      return false;
    }
  }

  /**
   * This TimerTask destroys the process that is passed to it.
   **/
  private class TPTimerTask extends TimerTask {
    Process p;
    public TPTimerTask(Process p) {
      this.p = p;
    }
    public void run() {
      try {
        int exit = p.exitValue();
        // System.out.println(); System.out.println("Process exited with status " + exit + ": " + command); System.out.println();
      } catch (IllegalThreadStateException ie) {
        p.destroy();
        // System.out.println("Process terminated after " + timeLimit + " msec: " + command); System.out.println();
      }
      this.cancel();
    }
  }
}
