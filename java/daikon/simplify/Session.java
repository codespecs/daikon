package daikon.simplify;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.*;
import java.util.*;

/*>>>
import org.checkerframework.checker.initialization.qual.*;
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * A session is a channel to the Simplify theorem-proving tool. Once a session is started, commands
 * may be applied to the session to make queries and manipulate its state.
 */
public class Session {
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

  // non-null if dkconfig_trace_input==true
  private /*@MonotonicNonNull*/ PrintStream trace_file;
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
    try {
      List<String> newEnv = new ArrayList<String>();
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
      process = java.lang.Runtime.getRuntime().exec(simplifyPath + " -nosc", envArray);
      SessionManager.debugln("Session: exec ok");

      if (dkconfig_trace_input) {
        File f;
        while ((f = new File("simplify" + trace_count + ".in")).exists()) trace_count++;
        trace_file = new PrintStream(new FileOutputStream(f));
      }

      // set up command stream
      @SuppressWarnings("nullness") // didn't redirect stream, so getter returns non-null
      PrintStream tmp_input = new PrintStream(process.getOutputStream());
      input = tmp_input;

      // set up result stream
      @SuppressWarnings("nullness") // didn't redirect stream, so getter returns non-null
      /*@NonNull*/ InputStream is = process.getInputStream();
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

    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  /* package access */ void sendLine(
      /*>>>@UnknownInitialization(Session.class) @Raw(Session.class) @GuardSatisfied Session this,*/ String
          s) {
    if (dkconfig_trace_input) {
      assert trace_file != null
          : "@AssumeAssertion(nullness): dependent: trace_file is non-null (set in constructor) if dkconfig_trace_input is true";
      trace_file.println(s);
    }
    input.println(s);
    input.flush();
  }

  /*@Holding("this")*/
  /* package access */
  /*@Nullable*/ String readLine(/*>>>@GuardSatisfied Session this*/) throws IOException {
    return output.readLine();
  }

  /*@Holding("this")*/
  public void kill(/*>>>@GuardSatisfied Session this*/) {
    process.destroy();
    if (dkconfig_trace_input) {
      assert trace_file != null
          : "@AssumeAssertion(nullness): conditional: trace_file is non-null if dkconfig_trace_input==true";
      trace_file.close();
    }
  }

  // for testing and playing around, not for real use
  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    /*@GuardedBy("<self>")*/ Session s = new Session();

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
