package daikon.simplify;

import java.io.IOException;

import utilMDE.Assert;

import org.apache.log4j.Logger;

/**
 * A Check command takes a given proposition and asks the Session to
 * prove it.  The apply method returns when a result is available; the
 * valid field contains the result.
 **/
public class CmdCheck
  implements Cmd
{
  public static final Logger debug = Logger.getLogger("daikon.simplify.CmdCheck");

  public final String proposition;
  public boolean valid = false;
  public String counterexample = "";

  public CmdCheck(String proposition) {
    this.proposition = proposition.trim();
    SimpUtil.assert_well_formed(proposition);
  }

  /** Read the class overview */
  public void apply(Session s) {
    try {

      String result;
      synchronized(s) {
        // send out the proposition
        s.input.println(proposition);
        s.input.flush();

        // read the answer
        // first, the real result
        result = s.output.readLine();
        if (debug.isDebugEnabled()) {
          debug.debug ("First line: " + result);
        }
        if (result == null) {
          throw new SimplifyError("Probable core dump");
        }
        if (result.startsWith("Bad input:")) {
          throw new SimplifyError(result + "\n" + proposition);
        }
        if (result.equals("Abort (core dumped)")) {
          throw new SimplifyError(result);
        }
        if (result.equals("Counterexample:")) {
          // Suck in the counterexample, if given
          do {
            counterexample += result + "\n";
            result = s.output.readLine();
            if (result == null) {
              throw new SimplifyError("Probable core dump");
            }
          } while (result.startsWith(" ") || result.startsWith("\t")
                   || result.equals(""));
        }
        // then, a blank line
        String blank = s.output.readLine();
        if (!("".equals(blank))) {
          throw new SimplifyError ("Not a blank line '" + blank +
                                   "' after output '" + result + "'");

        }
      }

      // expect "##: [Inv|V]alid."
      int colon = result.indexOf(": ");
      Assert.assertTrue(colon != -1);
      try {
        int junk = Integer.parseInt(result.substring(0, colon));
      } catch (NumberFormatException e) {
        Assert.assertTrue(false, "Expected number to prefix result '" + result + "'");
      }
      result = result.substring(colon + 2);
      if ("Valid.".equals(result)) {
        valid = true;
      } else {
        Assert.assertTrue("Invalid.".equals(result));
        valid = false;
      }

      SessionManager.debugln("Result: " + valid);
    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  public String toString() {
    return "CmdCheck: " + proposition;
  }

}
