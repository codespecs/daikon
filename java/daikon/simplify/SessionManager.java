package daikon.simplify;

import java.io.*;
import java.util.*;

import utilMDE.Assert;

/**
 * A SessionManager is a component which handles the threading
 * interaction with the Session.
 **/
public class SessionManager
{
  /** The command to be performed (point of communication with worker thread). */
  private Cmd pending;

  /** Our worker thread; hold onto it so that we can stop it. */
  private Worker worker;

  /** How long to wait for a reply for each command. */
  private int msec = 500;

  // The error message returned by the worked thread, or null
  private String error = null;

  // Enable to dump input and output to the console
  // Use "java -DDEBUG_SIMPLIFY=1 daikon.Daikon ..." or
  //     "make USER_JAVA_FLAGS=-DDEBUG_SIMPLIFY=1 ..."
  private static final boolean debug_mgr;
  static {
    debug_mgr = (System.getProperty("DEBUG_SIMPLIFY") != null);
    // debug_mgr = true;
    if (debug_mgr) {
      System.err.println("DEBUG_SIMPLIFY is on");
    }
  }
  public static void debugln(String s) {
    if (!debug_mgr) return;
    System.err.println(s);
    System.err.flush();
  }

  public SessionManager() {
    debugln("Creating SessionManager");
    worker = new Worker();
    worker.setDaemon(true);
    debugln("Manager: starting worker");
    worker.start();
    // We need to pause until the worked thread is blocked on the wait
    // call.  The next line does this, but is a really bad approach.
    // We need to figure out something better.
    try { Thread.currentThread().sleep(50); } catch (InterruptedException e) { }
    debugln("SessionManager created");
  }

  /**
   * Performs the given command, or times out if too much time
   * elapses.
   **/
  public void request(Cmd command)
    throws TimeoutException
  {
    Assert.assert(worker != null, "Cannot use closed SessionManager");
    Assert.assert(pending == null, "Cannot queue requests");
    if (debug_mgr) {
      System.err.println("Running command " + command);
      System.err.flush();
    }
    synchronized (this) {
      // place the command in the slot
      pending = command;
      // tell the worker to wake up
      this.notify();
      // wait for worker to finish
      try { this.wait(msec); } catch (InterruptedException e) { }
      // command finished iff the command was nulled out
      if (pending != null) {
	session_done();
	throw new TimeoutException();
      }
      // check for error
      if (error != null) {
	throw new SimplifyError(error);
      }
    }
  }

  /**
   * Shutdown this session.  No further commands may be executed.
   **/
  public void session_done() {
    worker.session_done();
    worker = null;
  }

  /**
   * Helper thread which interacts with a Session, according to the
   * enclosing manager.
   **/
  private class Worker
    extends Thread
  {
    private SessionManager mgr = SessionManager.this; // just sugar

    /** The associated session, or null if the thread should shutdown */
    private Session session = new Session();

    public void run() {
      debugln("Worker: run");
      synchronized (mgr) {
	while (session != null) {
	  try { mgr.wait(); } catch (InterruptedException e) { }
	  if (session != null && mgr.pending != null) {
	    error = null;
	    try {
	      mgr.pending.apply(session);
	    } catch (SimplifyError e) {
	      debugln(e.toString());
	      // Cause a timeout exception by not setting pending to
	      // null.  This might not be exactly the right thing to
	      // do, but a core dump from Simplify is arguably the
	      // same as a timeout.
	      mgr.notify();
	      continue;
	    } catch (Throwable e) {
	      error = e.toString();
	      e.printStackTrace();
	    }
	    mgr.pending = null;
	    mgr.notify();
	  }
	}
      }
    }

    private void session_done() {
      Session tmp = session;
      session = null;
      tmp.process.destroy();
    }
  }

  public static void main(String[] args)
    throws Exception
  {
    SessionManager m = new SessionManager();
    CmdCheck cc;

    cc = new CmdCheck("(EQ 1 1)");
    m.request(cc);
    Assert.assert(true == cc.valid);

    cc = new CmdCheck("(EQ 1 2)");
    m.request(cc);
    Assert.assert(false == cc.valid);

    cc = new CmdCheck("(EQ x z)");
    m.request(cc);
    Assert.assert(false == cc.valid);

    CmdAssume a = new CmdAssume("(AND (EQ x y) (EQ y z))");
    m.request(a);

    m.request(cc);
    Assert.assert(true == cc.valid);

    m.request(CmdUndoAssume.single);

    m.request(cc);
    Assert.assert(false == cc.valid);

  }

}
