package daikon.simplify;

import java.io.IOException;

import utilMDE.Assert;

/**
 * A Check command takes a given proposition and asks the Session to
 * prove it.  The apply method returns when a result is available; the
 * valid field contains the result.
 **/
public class CmdCheck
  implements Cmd
{
  public final String proposition;
  public boolean valid = false;
    
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
	if (result == null) {
	  throw new SimplifyError("Probable core dump");
	}
	Assert.assert(!result.startsWith("Bad input:"), result);
	if (result.equals("Abort (core dumped)")) {
	  throw new SimplifyError(result);
	}
	// then, a blank line
	String blank = s.output.readLine();
	Assert.assert("".equals(blank), "Not a blank line '" + blank +
		      "' after output '" + result + "'");
      }

      // expect "##: [Inv|V]alid."
      int colon = result.indexOf(": ");
      Assert.assert(colon != -1);
      try {
	int junk = Integer.parseInt(result.substring(0, colon));
      } catch (NumberFormatException e) {
	Assert.assert(false, "Expected number to prefix result '" + result + "'");
      }
      result = result.substring(colon + 2);
      if ("Valid.".equals(result)) {
	valid = true;
      } else {
	Assert.assert("Invalid.".equals(result));
	valid = false;
      }
	
    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  public String toString() {
    return "CmdCheck: " + proposition;
  }

}
