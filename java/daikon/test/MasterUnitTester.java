package daikon.test;

import daikon.FileIO;
import daikon.LogHelper;
import junit.framework.*;
import junit.textui.*;

/**
 * This class runs all the Daikon unit tests. These tests are small, fast to run, and certainly not
 * comprehensive. Daikon also has a much more substantial set of regression tests. The regression
 * tests appear at daikon/tests/ in the repository.
 */
public class MasterUnitTester extends TestCase {

  public static void main(String[] args) {
    FileIO.new_decl_format = Boolean.TRUE;
    TestRunner runner = new TestRunner();
    TestResult result = runner.doRun(suite(), false);
    if (!result.wasSuccessful()) {
      throw new daikon.Daikon.BugInDaikon("Unsuccessful test!");
    }
  }

  public MasterUnitTester(String name) {
    super(name);
  }

  public static Test suite() {
    LogHelper.setupLogs(LogHelper.INFO);

    TestSuite result = new TestSuite();

    // To determine what should be in this list:
    //   find . -name '*Test*.java'
    //     | perl -pe 's/^\./      daikon.test/; s:/:.:g; s/.java/.class,/;'
    //     | grep -v MasterUnitTester | sort

    @SuppressWarnings({"unchecked", "rawtypes"})
    Class<? extends TestCase>[] classes = (Class<? extends TestCase>[]) new Class[] {};

    for (int i = 0; i < classes.length; i++) {
      result.addTest(new TestSuite(classes[i]));
    }

    return result;
  }
}
