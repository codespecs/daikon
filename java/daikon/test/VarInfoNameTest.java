package daikon.test;

import java.io.*;
import java.util.*;
import junit.framework.*;

import daikon.*;

public class VarInfoNameTest
  extends TestCase
{

  // for convenience
  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(VarInfoNameTest.class));
  }

  public VarInfoNameTest(String name) {
    super(name);
  }

  public void testParse() { run("testParse"); }

  private void run(String name) {
    String file = "varInfoNameTest." + name;
    InputStream input_stream = VarInfoNameTest.class.getResourceAsStream(file);
    InputStream goal_stream = VarInfoNameTest.class.getResourceAsStream(file + ".goal");

    // run the tests
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    VarInfoNameDriver.run(input_stream, new PrintStream(out));

    // put output into actual
    List _actual = new ArrayList();
    StringTokenizer tok = new StringTokenizer(out.toString(), "\r\n");
    while (tok.hasMoreTokens()) {
      _actual.add(tok.nextToken());
    }
    String[] actual = (String[]) _actual.toArray(new String[_actual.size()]);

    // put desired into goal
    List _goal = new ArrayList();
    try {
      BufferedReader buf = new BufferedReader(new InputStreamReader(goal_stream));
      while (buf.ready()) {
	String line = buf.readLine();
	_goal.add(line);
      }
    } catch (IOException e) {
      throw new RuntimeException(e.toString());
    }
    String[] goal = (String[]) _goal.toArray(new String[_goal.size()]);

    // diff desired and output
    diff(goal, actual);
  }

  private void diff(String[] goal, String[] actual) {
    for (int i=0; i < goal.length; i++) {
      String goal_line = goal[i];
      if (i >= actual.length) {
	fail("Diff error:\nActual had too few lines, starting with goal line:\n\t" + goal_line);
      }
      String actual_line = actual[i];
      if (!goal_line.equals(actual_line)) {
	String goals = "";
	String actuals = "";
	int low = Math.max(0, i-3);
	int high = Math.min(Math.min(i+3, actual.length), goal.length);
	for (int j = low; j <= high; j++) {
	  if (!goal[j].equals(actual[j])) {
	    goals += ">";
	    actuals += ">";
	  }
	  goals += "\t" + goal[j] + "\n";
	  actuals += "\t" + actual[j] + "\n";
	}
	fail("Diff error:\nDifferent output encountered.  Expected:\n" +
	     goals + "Received:\n" + actuals);
      }
    }
    if (actual.length > goal.length) {
      fail("Diff error:\nActual had extra lines, starting with:\n\t" + actual[goal.length]);
    }
  }

  // parsing
  // interning
  // *name()
  // object methods

  // Simple
  // Size
  // Function
  // TypeOf
  // Prestate
  // Poststate
  // Add
  // Elements
  // Subscript
  // Slice

  // ElementsFinder
  // Replacer
  // InorderFlattener
  // QuantifierVisitor
  // QuantHelper.format_esc
}
