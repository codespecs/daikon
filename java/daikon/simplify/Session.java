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

  // for testing and playing around, not for real use
  public static void main(String[] args)
    throws IOException
  {
    Session s = new Session();
    System.out.println("" + s.check("(EQ 1 1)"));
    System.out.println("" + s.check("(EQ 1 2)"));
    System.out.flush();
  }

}
