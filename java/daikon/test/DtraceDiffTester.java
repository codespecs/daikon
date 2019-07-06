package daikon.test;

import daikon.*;
import daikon.tools.DtraceDiff;
import java.net.URL;
import junit.framework.*;

public class DtraceDiffTester extends TestCase {

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(DtraceDiffTester.class));
  }

  public DtraceDiffTester(String name) {
    super(name);
  }

  private static boolean diff(String file1, String file2) {
    // System.out.println("Diff: " + file1 + " " + file2);
    return DtraceDiff.mainTester(new String[] {find(file1), find(file2)});
  }

  private static boolean diff(String option, String optval, String file1, String file2) {
    // System.out.println("Diff: " + file1 + " " + file2);
    return DtraceDiff.mainTester(new String[] {option, optval, find(file1), find(file2)});
  }

  /**
   * Returns the URL string for the specified file. A URL is used because the tests can be run from
   * a jarfile and the 'filename' in that case is a jar URL.
   */
  private static String find(String file) {
    String file1 = "daikon/test/dtracediff/" + file;
    URL input_file_location = ClassLoader.getSystemResource(file1);
    if (input_file_location == null) {
      throw new Error("Cannot find " + file1);
    }
    return input_file_location.toExternalForm();
  }

  public void test_samples() {
    // these tests should succeed
    assert diff("AllTypes.dtrace.gz", "AllTypes.dtrace.gz");
    assert diff("Hanoi.dtrace.gz", "Hanoi.dtrace.gz");
    assert diff("Hanoi.dtrace.gz", "Hanoi-mungpointers.dtrace.gz");

    // test for the diffs that this utility is supposed to find
    assert !diff("Hanoi.dtrace.gz", "Hanoi-badvar.dtrace.gz");
    assert !diff("Hanoi.dtrace.gz", "Hanoi-badvalue.dtrace.gz");
    assert !diff("Hanoi.dtrace.gz", "Hanoi-truncated.dtrace.gz");

    // test that command-line options work (to avoid comparing ppts with
    // a missing variable)
    assert diff(
        "--ppt-omit-pattern",
        "six170.Hanoi.showTowers*",
        "Hanoi.dtrace.gz",
        "Hanoi-badvar.dtrace.gz");
    assert diff("--var-omit-pattern", "this.height", "Hanoi.dtrace.gz", "Hanoi-badvar.dtrace.gz");
    assert diff(
        "--ppt-select-pattern",
        "six170.Hanoi.moveDisk*",
        "Hanoi.dtrace.gz",
        "Hanoi-badvar.dtrace.gz");
    // needs to test --var-select-pattern
  }
}
