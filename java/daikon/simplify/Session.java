package daikon.simplify;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.INFO;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import org.checkerframework.checker.calledmethods.qual.EnsuresCalledMethods;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.lock.qual.GuardedBy;
import org.checkerframework.checker.lock.qual.Holding;
import org.checkerframework.checker.mustcall.qual.MustCall;
import org.checkerframework.checker.mustcall.qual.Owning;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * A session is a channel to the Simplify theorem-proving tool. Once a session is started, commands
 * may be applied to the session to make queries and manipulate its state.
 */
@MustCall("close") public class Session implements Closeable {
  /**
   * A non-negative integer, representing the largest number of iterations for which Simplify should
   * be allowed to run on any single conjecture before giving up. Larger values may cause Simplify
   * to run longer, but will increase the number of invariants that can be recognized as redundant.
   * The default value is small enough to keep Simplify from running for more than a few seconds on
   * any one conjecture, allowing it to verify most simple facts without getting bogged down in long
   * searches. A value of 0 means not to bound the number of iterations at all, though see also the
   * {@code simplify_timeout} parameter..
   */
  public static int dkconfig_simplify_max_iterations = 1000;

  /**
   * A non-negative integer, representing the longest time period (in seconds) Simplify should be
   * allowed to run on any single conjecture before giving up. Larger values may cause Simplify to
   * run longer, but will increase the number of invariants that can be recognized as redundant.
   * Roughly speaking, the time spent in Simplify will be bounded by this value, times the number of
   * invariants generated, though it can be much less. A value of 0 means to not bound Simplify at
   * all by time, though also see the option {@code simplify_max_iterations}. Beware that using this
   * option might make Daikon's output depend on the speed of the machine it's run on.
   */
  public static int dkconfig_simplify_timeout = 0;

  /**
   * Positive values mean to print extra indications as each candidate invariant is passed to
   * Simplify during the {@code --suppress_redundant} check. If the value is 1 or higher, a hyphen
   * will be printed when each invariant is passed to Simplify, and then replaced by a {@code T} if
   * the invariant was redundant, {@code F} if it was not found to be, and {@code ?} if Simplify
   * gave up because of a time limit. If the value is 2 or higher, a {@code <} or {@code >} will
   * also be printed for each invariant that is pushed onto or popped from from Simplify's
   * assumption stack. This option is mainly intended for debugging purposes, but can also provide
   * something to watch when Simplify takes a long time.
   */
  public static int dkconfig_verbose_progress = 0;

  /**
   * Boolean. If true, the input to the Simplify theorem prover will also be directed to a file
   * named simplifyN.in (where N is a number starting from 0) in the current directory. Simplify's
   * operation can then be reproduced with a command like {@code Simplify -nosc <simplify0.in}. This
   * is intended primarily for debugging when Simplify fails.
   */
  public static boolean dkconfig_trace_input = false;

  /** non-null if dkconfig_trace_input==true */
  private final @Owning @MonotonicNonNull PrintStream trace_file;

  /** A unique identifier for creating unique filenames for trace files. */
  private static int trace_count = 0;

  /* package */ final Process process;
  private final PrintStream input;
  private final BufferedReader output;

  /**
   * Starts a new Simplify process, which runs concurrently; I/O with this process will block.
   * Initializes the simplify environment for interaction. Use {@code Cmd} objects to interact with
   * this Session.
   */
  public Session() {
    // Note that this local variable shadows `this.trace_file`.
    PrintStream trace_file = null;
    try {
      List<String> newEnv = new ArrayList<>();
      if (dkconfig_simplify_max_iterations != 0) {
        newEnv.add("PROVER_KILL_ITER=" + dkconfig_simplify_max_iterations);
      }
      if (dkconfig_simplify_timeout != 0) {
        newEnv.add("PROVER_KILL_TIME=" + dkconfig_simplify_timeout);
      }
      String[] envArray = newEnv.toArray(new String[] {});
      SessionManager.debugln("Session: exec");
      // -nosc: don't compute or print invalid context
      String simplifyPath;
      if (System.getProperty("simplify.path") == null) {
        simplifyPath = "Simplify";
      } else {
        simplifyPath = System.getProperty("simplify.path");
      }
      process = java.lang.Runtime.getRuntime().exec(new String[] {simplifyPath, "-nosc"}, envArray);
      SessionManager.debugln("Session: exec ok");

      if (dkconfig_trace_input) {
        File f;
        while ((f = new File("simplify" + trace_count + ".in")).exists()) {
          trace_count++;
        }
        trace_file = new PrintStream(new FileOutputStream(f));
      }
      this.trace_file = trace_file;

      // set up command stream
      PrintStream tmp_input = new PrintStream(process.getOutputStream());
      input = tmp_input;

      // set up result stream
      @NonNull InputStream is = process.getInputStream();
      output = new BufferedReader(new InputStreamReader(is, UTF_8));

      // turn off prompting
      SessionManager.debugln("Session: prompt off");
      sendLine("(PROMPT_OFF)");
      SessionManager.debugln("Session: eat prompt");
      // eat first (and only, because we turn it off) prompt
      String expect = ">\t";
      byte[] buf = new byte[expect.length()];
      int pos = is.read(buf);
      assert pos != -1 : "Prompt exected, stream ended";
      String actual = new String(buf, 0, pos, UTF_8);
      assert expect.equals(actual) : "Prompt expected, got '" + actual + "'";

    } catch (Exception | AssertionError e) {
      if (trace_file != null) {
        try {
          trace_file.close();
        } catch (Exception closeException) {
          e.addSuppressed(closeException);
        }
      }
      throw new SimplifyError(e);
    }
  }

  /* package access */ void sendLine(
      @UnknownInitialization(Session.class) @GuardSatisfied Session this, String s) {
    if (dkconfig_trace_input) {
      assert trace_file != null
          : "@AssumeAssertion(nullness): dependent: trace_file is non-null (set in constructor) if"
              + " dkconfig_trace_input is true";
      trace_file.println(s);
    }
    input.println(s);
    input.flush();
  }

  @Holding("this")
  /* package access */
  @Nullable String readLine(@GuardSatisfied Session this) throws IOException {
    return output.readLine();
  }

  /** Releases the resources held by this. */
  @EnsuresCalledMethods(value = "trace_file", methods = "close")
  @Override
  public void close(@GuardSatisfied Session this) {
    process.destroy();
    assert dkconfig_trace_input == (trace_file != null)
        : "@AssumeAssertion(nullness): conditional: trace_file is non-null if"
            + " dkconfig_trace_input==true";
    if (trace_file != null) {
      trace_file.close();
    }
  }

  /**
   * for testing and playing around, not for real use
   *
   * @param args command-line arguments
   */
  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(INFO);
    try (@GuardedBy("<self>") Session s = new Session()) {

      CmdCheck cc;

      cc = new CmdCheck("(EQ 1 1)");
      cc.apply(s);
      assert cc.valid;

      cc = new CmdCheck("(EQ 1 2)");
      cc.apply(s);
      assert !cc.valid;

      cc = new CmdCheck("(EQ x z)");
      cc.apply(s);
      assert !cc.valid;

      CmdAssume a = new CmdAssume("(AND (EQ x y) (EQ y z))");
      a.apply(s);

      cc.apply(s);
      assert cc.valid;

      CmdUndoAssume.single.apply(s);

      cc.apply(s);
      assert !cc.valid;
    }
  }
}
