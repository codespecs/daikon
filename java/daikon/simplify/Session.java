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
      // -nosc: don't compute or print invalid context
      process = Runtime.getRuntime().exec("Simplify -nosc");

      // set up command stream and turn off prompting
      input = new PrintStream(process.getOutputStream());
      input.println("(PROMPT_OFF)");
      input.flush();

      // eat first prompt
      InputStream is = process.getInputStream();
      String expect = ">\t";
      byte[] buf = new byte[expect.length()];
      int pos = is.read(buf);
      String actual = new String(buf, 0, pos);
      Assert.assert(expect.equals(actual), "Prompt expected, got '" + actual + "'");

      // set up result stream
      output = new BufferedReader(new InputStreamReader(is));
      
    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  // for testing and playing around, not for real use
  public static void main(String[] args)
    throws IOException
  {
    Session s = new Session();

    CmdCheck cc;

    cc = new CmdCheck("(EQ 1 1)");
    cc.apply(s);
    Assert.assert(true == cc.valid);

    cc = new CmdCheck("(EQ 1 2)");
    cc.apply(s);
    Assert.assert(false == cc.valid);

    cc = new CmdCheck("(EQ x z)");
    cc.apply(s);
    Assert.assert(false == cc.valid);

    CmdAssume a = new CmdAssume("(AND (EQ x y) (EQ y z))");
    a.apply(s);

    cc.apply(s);
    Assert.assert(true == cc.valid);

    CmdUndoAssume.single.apply(s);

    cc.apply(s);
    Assert.assert(false == cc.valid);
  }

}
