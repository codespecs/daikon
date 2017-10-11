package daikon.simplify;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

/** A SessionManager is a component which handles the threading interaction with the Session. */
public class SessionManager {
  /** The command to be performed (point of communication with worker thread). */
  private /*@Nullable*/ Cmd pending;

  /** Our worker thread; hold onto it so that we can stop it. */
  private Worker worker;

  // The error message returned by the worked thread, or null
  private /*@Nullable*/ String error = null;

  /** Debug tracer common to all Simplify classes. */
  public static final Logger debug = Logger.getLogger("daikon.simplify");

  // Deprecated method for setting the debug flag.
  //    // Enable to dump input and output to the console
  //    // Use "java -DDEBUG_SIMPLIFY=1 daikon.Daikon ..." or
  //    //     "make USER_JAVA_FLAGS=-DDEBUG_SIMPLIFY=1 ..."

  private static final boolean debug_mgr = debug.isLoggable(Level.FINE);

  public static void debugln(String s) {
    if (!debug_mgr) return;
    debug.fine(s);
  }

  public SessionManager() {
    debugln("Creating SessionManager");
    worker = new Worker();
    worker.setDaemon(true);
    debugln("Manager: starting worker");
    synchronized (this) {
      worker.start();
      // We won't wake up from this until the worker thread is ready
      // and wait()ing to accept requests.
      try {
        this.wait();
      } catch (InterruptedException e) {
      }
    }
    debugln("SessionManager created");
  }

  /** Performs the given command, or times out if too much time elapses. */
  public void request(Cmd command) throws TimeoutException {
    assert worker != null : "Cannot use closed SessionManager";
    assert pending == null : "Cannot queue requests";
    if (debug.isLoggable(Level.FINE)) {
      System.err.println("Running command " + command);
      System.err.println(" called from");
      Throwable t = new Throwable();
      t.printStackTrace();
      System.err.flush();
    }
    synchronized (this) {
      // place the command in the slot
      assert pending == null;
      pending = command;
      // tell the worker to wake up
      this.notifyAll();
      // wait for worker to finish
      try {
        this.wait();
      } catch (InterruptedException e) {
      }
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

  /** Shutdown this session. No further commands may be executed. */
  @SuppressWarnings("nullness") // nulling worker for fast failure (& for GC)
  public void session_done() {
    worker.session_done();
    worker = null;
  }

  private static /*@MonotonicNonNull*/ String prover_background = null;

  private static String proverBackground() {
    if (prover_background == null) {
      try {
        StringBuffer result = new StringBuffer("");
        String fileName;
        if (daikon.inv.Invariant.dkconfig_simplify_define_predicates) {
          fileName = "daikon-background-defined.txt";
        } else {
          fileName = "daikon-background.txt";
        }
        InputStream bg_stream = SessionManager.class.getResourceAsStream(fileName);
        if (bg_stream == null) {
          throw new RuntimeException(
              "Could not find resource daikon/simplify/" + fileName + " on the classpath");
        }
        BufferedReader lines = new BufferedReader(new InputStreamReader(bg_stream, UTF_8));
        String line;
        while ((line = lines.readLine()) != null) {
          line = line.trim();
          if ((line.length() == 0) || line.startsWith(";")) {
            continue;
          }
          result.append(" ");
          result.append(line);
          result.append(daikon.Global.lineSep);
        }
        lines.close();
        prover_background = result.toString();
      } catch (IOException e) {
        throw new RuntimeException("Could not load prover background");
      }
    }
    return prover_background;
  }

  public static int prover_instantiate_count = 0;

  // Start up simplify, and send the universal backgound.
  // Is successful exactly when return != null.
  public static /*@Nullable*/ SessionManager attemptProverStartup() {
    SessionManager prover;

    // Limit ourselves to a few tries
    if (prover_instantiate_count > 5) {
      return null;
    }

    // Start the prover
    try {
      prover_instantiate_count++;
      prover = new SessionManager();
      if (daikon.Daikon.no_text_output) {
        System.out.print("...");
      }
    } catch (SimplifyError e) {
      System.err.println("Could not utilize Simplify: " + e);
      return null;
    }

    try {
      prover.request(new CmdRaw(proverBackground()));
    } catch (TimeoutException e) {
      throw new RuntimeException("Timeout on universal background", e);
    }
    return prover;
  }

  /** Helper thread which interacts with a Session, according to the enclosing manager. */
  private class Worker extends Thread {
    private final SessionManager mgr = SessionManager.this; // just sugar

    /** The associated session, or null if the thread should shutdown. */
    private /*@Nullable*/ /*@GuardedBy("<self>")*/ Session session = new Session();

    private boolean finished = false;

    @Override
    @SuppressWarnings("nullness") // tricky, but I think it's OK
    public void run() {
      debugln("Worker: run");
      while (session != null) {
        synchronized (mgr) {
          mgr.pending = null;
          mgr.notifyAll();
          try {
            mgr.wait(0);
          } catch (InterruptedException e) {
          }
          assert mgr.pending != null : "@AssumeAssertion(nullness)";
          // session != null && mgr.pending != null;
        }
        error = null;
        try {
          // session could also be null at this point, I presume.
          // That's probably what the catch block is for.
          mgr.pending.apply(session);
        } catch (Throwable e) {
          if (finished) return;
          error = e.toString();
          e.printStackTrace();
        }
      }
    }

    /*@RequiresNonNull("session")*/
    private void session_done() {
      finished = true;
      final /*@GuardedBy("<self>")*/ Session tmp = session;
      session = null;
      synchronized (tmp) {
        tmp.kill();
      }
    }
  }

  public static void main(String[] args) throws Exception {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    SessionManager m = new SessionManager();
    CmdCheck cc;

    cc = new CmdCheck("(EQ 1 1)");
    m.request(cc);
    assert cc.valid;

    cc = new CmdCheck("(EQ 1 2)");
    m.request(cc);
    assert !cc.valid;

    cc = new CmdCheck("(EQ x z)");
    m.request(cc);
    assert !cc.valid;

    CmdAssume a = new CmdAssume("(AND (EQ x y) (EQ y z))");
    m.request(a);

    m.request(cc);
    assert cc.valid;

    m.request(CmdUndoAssume.single);

    m.request(cc);
    assert !cc.valid;

    StringBuffer buf = new StringBuffer();

    for (int i = 0; i < 20000; i++) {
      buf.append("(EQ (select a " + i + ") " + (int) (200000 * Math.random()) + ")");
    }
    m.request(new CmdAssume(buf.toString()));

    for (int i = 0; i < 10; i++) {
      try {
        m.request(new CmdCheck("(NOT (EXISTS (x) (EQ (select a x) (+ x " + i + "))))"));
      } catch (TimeoutException e) {
        System.out.println("Timeout, retrying");
        m = new SessionManager();
        m.request(new CmdAssume(buf.toString()));
      }
    }
  }
}
