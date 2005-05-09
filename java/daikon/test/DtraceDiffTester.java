package daikon.test;

import junit.framework.*;
import daikon.*;
import daikon.tools.DtraceDiff;
import java.lang.reflect.*;
import java.net.URL;

public class DtraceDiffTester extends TestCase {

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(DtraceDiffTester.class));
  }

  public DtraceDiffTester(String name) {
    super(name);
  }

  private static boolean diff(String file1, String file2) {
    //System.out.println("Diff: " + file1 + " " + file2);
    return DtraceDiff.mainTester(new String[] {find(file1), find(file2)});
  }

  private static String find(String file) {
    String file1 = "daikon/test/dtracediff/" + file;
    URL input_file_location =
      ClassLoader.getSystemClassLoader().getSystemResource(file1);
    assertTrue(input_file_location != null);
    return input_file_location.getFile();
  }

  public void test_samples () {
    assertTrue(diff("Hanoi.dtrace.gz", "Hanoi.dtrace.gz"));
    assertTrue(diff("Hanoi.dtrace.gz", "Hanoi-mungpointers.dtrace.gz"));
    assertFalse(diff("Hanoi.dtrace.gz", "Hanoi-badvar.dtrace.gz"));
    assertFalse(diff("Hanoi.dtrace.gz", "Hanoi-badvalue.dtrace.gz"));
    assertFalse(diff("Hanoi.dtrace.gz", "Hanoi-truncated.dtrace.gz"));
  }

}
