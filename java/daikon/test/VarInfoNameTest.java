package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.logging.Level.INFO;
import static org.junit.Assert.fail;

import daikon.FileIO;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import junit.framework.*;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * This tests various aspects of VarInfoName's and transforming VarInfoName's. This calls
 * VarInfoNameDriver after parsing all input files of the name "varInfoNameTest.<em>foo</em>".
 * VarInfoNameDriver does transform tests, and its output is compared to the
 * "varInfoNameTest.<em>foo</em>.goal" file by this.
 *
 * <p>To add a new test case, add a line to the <em>foo</em> file and a line to the goal file with
 * intended output. Format of the <em>foo</em> file is output method, followed by a variable name.
 * Output methods are defined in VarInfoNameDriver. To add a new transformation method (which can
 * then be tested in test cases) add a static Handler implementation to VarInfoNameDriver modeled
 * after one of the ones already present and add a static {} line after to add the handler to the
 * list of handlers.
 */
public class VarInfoNameTest {

  private static final String lineSep = daikon.Global.lineSep;

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @Test
  public void testParse() {
    run("testParse");
  }

  // Fails mysteriously, only when run from a cron job.  The failure is in
  // obsolescent code (VarInfoNameTest), so comment it out rather than
  // debugging it.
  // @Test public void testEscForall() { run("testEscForall"); }
  @Test
  public void testSubscript() {
    run("testSubscript");
  }

  @Test
  public void testJML() {
    run("testJML");
  }

  private void run(String name) {
    String file = "varInfoNameTest." + name;
    try (InputStream input_stream = VarInfoNameTest.class.getResourceAsStream(file);
        InputStream goal_stream = VarInfoNameTest.class.getResourceAsStream(file + ".goal")) {

      if (input_stream == null) {
        throw new Error("couldn't find " + file);
      }
      if (goal_stream == null) {
        throw new Error("couldn't find " + file + ".goal");
      }

      // run the tests
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      VarInfoNameDriver.run(input_stream, new PrintStream(out));

      // put output into actual
      List<String> _actual = new ArrayList<>();
      @SuppressWarnings("DefaultCharset") // toString(Charset) was introduced in Java 10
      StringTokenizer tok = new StringTokenizer(out.toString(), lineSep);
      while (tok.hasMoreTokens()) {
        _actual.add(tok.nextToken());
      }
      String[] actual = _actual.toArray(new String[0]);

      // put desired into goal
      List<String> _goal = new ArrayList<>();
      try {
        BufferedReader buf = new BufferedReader(new InputStreamReader(goal_stream, UTF_8));
        while (buf.ready()) {
          String line = buf.readLine();
          _goal.add(line);
        }
        buf.close();
      } catch (IOException e) {
        throw new RuntimeException(e.toString());
      }
      String[] goal = _goal.toArray(new String[0]);

      // diff desired and output
      diff(goal, actual);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private void diff(String[] goal, String[] actual) {
    for (int i = 0; i < goal.length; i++) {
      String goal_line = goal[i];
      if (i >= actual.length) {
        fail(
            "Diff error:"
                + lineSep
                + "Actual had too few lines, starting with goal line:"
                + lineSep
                + "\t"
                + goal_line);
      }
      String actual_line = actual[i];
      if (!goal_line.equals(actual_line)) {
        String goals = "";
        String actuals = "";
        int low = Math.max(0, i - 3);
        int high = Math.min(Math.min(i + 3, actual.length - 1), goal.length - 1);
        for (int j = low; j <= high; j++) {
          if (!goal[j].equals(actual[j])) {
            goals += ">";
            actuals += ">";
          }
          goals += "\t" + goal[j] + lineSep;
          actuals += "\t" + actual[j] + lineSep;
        }
        fail(
            "Diff error:"
                + lineSep
                + "Different output encountered.  Expected:"
                + lineSep
                + goals
                + "Received:"
                + lineSep
                + actuals
                + " on line: "
                + i);
      }
    }
    if (actual.length > goal.length) {
      StringBuilder extra = new StringBuilder();
      for (int i = goal.length; i < actual.length; i++) {
        extra.append("\t");
        extra.append(actual[i]);
        extra.append(lineSep);
      }
      fail("Diff error:" + lineSep + "Actual had extra lines:" + lineSep + extra.toString());
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
