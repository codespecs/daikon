package daikon.simplify;

import java.io.*;
import java.util.*;

import utilMDE.Assert;

/**
 * A session is a channel to the Simplify theorem-proving tool.  Once
 * a session started, commands may be applied to the session to make
 * queries and manipulate its state.
 **/
public class Session
{
  /**
   * A non-negative integer, representing the largest number of
   * iterations for which Simplify should be allowed to run on any
   * single conjecture before giving up. Larger values may cause
   * --simplify_redundant to run longer, but will increase the number
   * of invariants that can be recognized as redundant. The default
   * value is small enough to keep Simplify from running for more than
   * a few seconds on any one conjecture, allowing it to verify most
   * simple theorems without getting bogged down in long searches.
   **/
  public static int dkconfig_simplify_max_iterations = 1000000;

  /* package */ final Process process;
  /* package */ final PrintStream input;
  /* package */ final BufferedReader output;

  /**
   * Starts a new Simplify process, which runs concurrently; I/O with
   * this process will block.  Initializes the simplify environment
   * for interaction.  Use <code>Cmd</code> objects to interact with
   * this Session.
   **/
  public Session() {
    try {
      String killIter = dkconfig_simplify_max_iterations + "";
      String[] newEnv = new String[] { "PROVER_KILL_ITER=" + killIter};
      SessionManager.debugln("Session: exec");
      // -nosc: don't compute or print invalid context
      process = java.lang.Runtime.getRuntime().exec("Simplify -nosc", newEnv);
      SessionManager.debugln("Session: exec ok");

      SessionManager.debugln("Session: prompt off");
      // set up command stream and turn off prompting
      input = new PrintStream(process.getOutputStream());
      input.println("(PROMPT_OFF)");
      input.flush();

      SessionManager.debugln("Session: eat prompt");
      // eat first (and only, because we turn it off) prompt
      InputStream is = process.getInputStream();
      String expect = ">\t";
      byte[] buf = new byte[expect.length()];
      int pos = is.read(buf);
      String actual = new String(buf, 0, pos);
      Assert.assertTrue(expect.equals(actual),
                        "Prompt expected, got '" + actual + "'");

      // set up result stream
      output = new BufferedReader(new InputStreamReader(is));

    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  public void kill() {
    process.destroy();
  }

  // for testing and playing around, not for real use
  public static void main(String[] args)
    throws IOException
  {
    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    Session s = new Session();

    CmdCheck cc;

    cc = new CmdCheck("(EQ 1 1)");
    cc.apply(s);
    Assert.assertTrue(true == cc.valid);

    cc = new CmdCheck("(EQ 1 2)");
    cc.apply(s);
    Assert.assertTrue(false == cc.valid);

    cc = new CmdCheck("(EQ x z)");
    cc.apply(s);
    Assert.assertTrue(false == cc.valid);

    CmdAssume a = new CmdAssume("(AND (EQ x y) (EQ y z))");
    a.apply(s);

    cc.apply(s);
    Assert.assertTrue(true == cc.valid);

    CmdUndoAssume.single.apply(s);

    cc.apply(s);
    Assert.assertTrue(false == cc.valid);
  }

}
