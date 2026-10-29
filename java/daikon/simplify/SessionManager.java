package daikon.simplify;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.FINE;
import static java.util.logging.Level.INFO;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.logging.Logger;
import org.checkerframework.checker.calledmethods.qual.EnsuresCalledMethods;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/** A SessionManager is a component which handles the threading interaction with the Session. */
public class SessionManager implements Closeable {
  /** The session, which commands are applied to. */
  private final @Owning @GuardedBy("<self>") Session session;

  /** Applies commands to {@link #session}, one at a time, on a separate thread. */
  private final ExecutorService executor;

  /**
   * True if this has been closed. It is volatile because {@link #close} may write it on a thread
   * other than the one in {@link #request}.
   */
  private volatile boolean closed = false;

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
    session = new Session();
    executor =
        Executors.newSingleThreadExecutor(
            r -> {
              Thread t = new Thread(r);
              t.setDaemon(true);
              return t;
            });
    debugln("SessionManager created");
  }

  /**
   * Performs the given command.
   *
   * @param command the command to perform
   * @throws TimeoutException if another thread closes this before the command completes
   */
  public void request(Cmd command) throws TimeoutException {
    assert !closed : "Cannot use closed SessionManager";
    if (debug.isLoggable(FINE)) {
      System.err.println("Running command " + command);
      System.err.println(" called from");
      Throwable t = new Throwable();
      t.printStackTrace();
      System.err.flush();
    }
    Future<?> result;
    try {
      result = executor.submit(() -> command.apply(session));
    } catch (RejectedExecutionException e) {
      // Another thread closed this before the command was submitted.
      throw new TimeoutException();
    }
    while (true) {
      try {
        result.get();
        if (closed) {
          // Another thread closed this while the command was running.  The command may have
          // completed normally even though Simplify did not receive its input, because writes to
          // a closed session fail silently.
          throw new TimeoutException();
        }
        return;
      } catch (InterruptedException e) {
        // It's OK for a wait to be interrupted.
      } catch (CancellationException e) {
        // Another thread closed this before the command started.
        throw new TimeoutException();
      } catch (ExecutionException e) {
        if (closed) {
          // Another thread closed this while the command was running.
          throw new TimeoutException();
        }
        Throwable cause = e.getCause();
        assert cause != null
            : "@AssumeAssertion(nullness): a Future's ExecutionException has a cause";
        // Print the stack trace because some clients discard SimplifyError without reporting it.
        cause.printStackTrace();
        throw new SimplifyError(cause);
      }
    }
  }

  /**
   * Shutdown this session. No further commands may be executed. Has no effect if already closed.
   */
  @SuppressWarnings("lock:lock.not.held") // must not wait for a command that is blocked on Simplify
  @EnsuresCalledMethods(value = "session", methods = "close")
  @Override
  public void close(@GuardSatisfied SessionManager this) {
    closed = true;
    try {
      // Shut down the executor before closing `session`, so that no command starts on a closed
      // session.  Cancel commands that never started, so that `request()` does not wait for them
      // forever.
      for (Runnable unstarted : executor.shutdownNow()) {
        if (unstarted instanceof Future) {
          ((Future<?>) unstarted).cancel(false);
        }
      }
    } finally {
      // Do not acquire the lock on `session`:  a command may hold it while blocked reading from a
      // hung Simplify process, and only `session.close()` can unblock that command.
      session.close();
    }
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
        try (BufferedReader lines = new BufferedReader(new InputStreamReader(bg_stream, UTF_8))) {
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
        }
        prover_background = result.toString();
      } catch (IOException e) {
        throw new RuntimeException("Could not load prover background");
      }
    }
    return prover_background;
  }

  public static int prover_instantiate_count = 0;

  // Start up simplify, and send the universal background.
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
