package daikon.simplify;

import java.io.*;
import java.util.*;
    
import utilMDE.Assert;

/**
 * A session is a channel to the Simplify theorem-proving tool.
 * Propositions may be checked and reported true or false.  Background
 * predicates may also be temporarily assumed but later removed.
 **/
public class Session
{

  private final Process process;
  private final PrintStream input;
  private final BufferedReader output;

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
      int size = ">       ".length();
      is.read(new byte[size], 0, size);

      // set up result stream
      output = new BufferedReader(new InputStreamReader(is));
      
    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  public boolean check(String proposition) {
    proposition = proposition.trim();
    assert_well_formed(proposition);
    try {
      // send out the proposition
      input.println(proposition);
      input.flush();

      // read the answer
      String result = output.readLine();
      Assert.assert("".equals(output.readLine())); // eat blank line

      // expect "##: [Inv|V]alid."
      boolean valid;
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

      return valid;

    } catch (IOException e) {
      throw new SimplifyError(e.toString());
    }
  }

  public void assume(String proposition) {
    proposition = proposition.trim();
    assert_well_formed(proposition);

    // send out the (BG_PUSH proposition)
    input.println("(BG_PUSH " + proposition + ")");
    input.flush();

    // there is no output from Simplify
  }

  public void undo_last_assume() {
    // send out the (BG_POP)
    input.println("(BG_POP)");
    input.flush();
    
    // there is no output from Simplify
  }

  private static void assert_well_formed(String s) {
    if (!Assert.enabled) {
      return;
    }

    Assert.assert(s != null);
    Assert.assert(s.indexOf("((") == -1, "'((' may not appear");
    Assert.assert(s.charAt(0) == '(', "starts with lparen");
    Assert.assert(s.charAt(s.length()-1) == ')', "ends with rparen");

    int paren = 0;
    char[] cs = s.toCharArray();
    for (int i=0; i < cs.length; i++) {
      char c = cs[i];
      if (c == '(') {
	paren++;
      } else if (c == ')') {
	Assert.assert(paren > 0, "too deep at char " + i + " in '" + s + "'");
	paren--;
      }
    }
    Assert.assert(paren == 0, "unbalanced parens in '" + s + "'");
  }

  // for testing and playing around, not for real use
  public static void main(String[] args)
    throws IOException
  {
    Session s = new Session();

    Assert.assert(true == s.check("(EQ 1 1)"));
    Assert.assert(false == s.check("(EQ 1 2)"));

    Assert.assert(false == s.check("(EQ x z)"));
    s.assume("(AND (EQ x y) (EQ y z))");
    Assert.assert(true == s.check("(EQ x z)"));
    s.undo_last_assume();
    Assert.assert(false == s.check("(EQ x z)"));
  }

}
