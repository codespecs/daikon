package daikon.simplify;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.logging.Logger;
import org.checkerframework.checker.calledmethods.qual.EnsuresCalledMethods;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.checker.mustcall.qual.MustCall;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

/** A SessionManager is a component which handles the threading interaction with the Session. */
public class SessionManager implements Closeable {
  /** The command to be performed (point of communication with worker thread). */
  private @Nullable Cmd pending;

  /** Our worker thread; hold onto it so that we can stop it. */
  private @Owning Worker worker;

  // The error message returned by the worked thread, or null
  private @Nullable String error = null;

  /** Debug tracer common to all Simplify classes. */
  public static final Logger debug = Logger.getLogger("daikon.simplify");

  // Deprecated method for setting the debug flag.
  //    // Enable to dump input and output to the console
  //    // Use "java -DDEBUG_SIMPLIFY=1 daikon.Daikon ..." or
  //    //     "make USER_JAVA_FLAGS=-DDEBUG_SIMPLIFY=1 ..."

  /** debugging flag */
  private static final boolean debug_mgr = debug.isLoggable(FINE);

  /**
   * log debug message
   *
   * @param s message to log
   */
  public static void debugln(String s) {
    if (!debug_mgr) {
      return;
    }
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
        // It's OK for a wait() to be interrupted.
      }
    }
    debugln("SessionManager created");
  }

  /** Performs the given command, or times out if too much time elapses. */
  public void request(Cmd command) throws TimeoutException {
    assert worker != null : "Cannot use closed SessionManager";
    assert pending == null : "Cannot queue requests";
    if (debug.isLoggable(FINE)) {
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
        // It's OK for a wait() to be interrupted.
      }
      // command finished iff the command was nulled out
      if (pending != null) {
        close();
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
  @EnsuresCalledMethods(value = "worker", methods = "close")
  @Override
  public void close(@GuardSatisfied SessionManager this) {
    worker.close();
    worker = null;
  }

  private static @MonotonicNonNull String prover_background = null;

  private static String proverBackground() {
    if (prover_background == null) {
      try {
        StringBuilder result = new StringBuilder("");
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
  public static @Nullable SessionManager attemptProverStartup() {
    SessionManager prover;

    // Limit ourselves to a few tries
    if (prover_instantiate_count > 5) {
      return null;
    }

    // Start the prover
    try {
      prover_instantiate_count++;
      prover = new SessionManager();
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
  @MustCall("close") private class Worker extends Thread implements Closeable {
    /** The session mananger. */
    private final SessionManager mgr = SessionManager.this; // just sugar

    /** The associated session, or null if the thread should shutdown. */
    private @Owning @Nullable @GuardedBy("<self>") Session session = new Session();

    /** True if this has been closed. */
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
            // It's OK for a wait() to be interrupted.
          }
          assert mgr.pending != null
              : "@AssumeAssertion(nullness): bug? might not be true if interrupted?";
          // session != null && mgr.pending != null;
        }
        error = null;
        try {
          // session could also be null at this point, I presume.
          // That's probably what the catch block is for.
          mgr.pending.apply(session);
        } catch (Throwable e) {
          if (finished) {
            return;
          }
          error = e.toString();
          e.printStackTrace();
        }
      }
    }

    @SuppressWarnings({
      "nullness:contracts.precondition.override",
      "builder:required.method.not.called" // operation performed on alias
    })
    @EnsuresCalledMethods(value = "session", methods = "close")
    @RequiresNonNull("session")
    @Override
    public void close(@GuardSatisfied Worker this) {
      finished = true;
      final @GuardedBy("<self>") Session tmp = session;
      synchronized (tmp) {
        tmp.close();
      }
      session = null;
    }
  }

  /**
   * Entry point for testing.
   *
   * @param args command-line arguments
   * @throws TimeoutException if SessionManager times out
   */
  public static void main(String[] args) throws TimeoutException {
    daikon.LogHelper.setupLogs(INFO);
    SessionManager m = null; // dummy initialization to satisfy compiler's definite assignment check
    try {
      m = new SessionManager();
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

      StringBuilder buf = new StringBuilder();

      for (int i = 0; i < 20000; i++) {
        buf.append("(EQ (select a " + i + ") " + (int) (200000 * Math.random()) + ")");
      }
      m.request(new CmdAssume(buf.toString()));

      for (int i = 0; i < 10; i++) {
        try {
          m.request(new CmdCheck("(NOT (EXISTS (x) (EQ (select a x) (+ x " + i + "))))"));
        } catch (TimeoutException e) {
          System.out.println("Timeout, retrying");
          m.close();
          m = new SessionManager();
          m.request(new CmdAssume(buf.toString()));
        }
      }
    } finally {
      if (m != null) {
        m.close();
      }
    }
  }
}
