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

  private static final boolean debug_mgr = false;

  public SessionManager() {
    worker = new Worker();
    worker.setDaemon(true);
    worker.start();
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
    }
    synchronized (this) {
      // place the command in the slot
      pending = command;
      // tell the worker to wake up
      this.notify();
      // wait for worker to finish
      try { this.wait(msec); } catch (InterruptedException e) { }
      // ok iff the command was nulled out
      if (pending != null) {
	session_done();
	throw new TimeoutException();
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
      synchronized (mgr) {
	while (session != null) {
	  try { mgr.wait(); } catch (InterruptedException e) { }
	  if (session != null && mgr.pending != null) {
	    mgr.pending.apply(session);
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
