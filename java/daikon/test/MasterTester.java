package daikon.test;

import daikon.Logger;

import junit.framework.*;
import junit.textui.*;
import utilMDE.*;

public class MasterTester extends TestCase {

  public static void main(String[] args) {
    TestRunner runner = new TestRunner();
    TestResult result = runner.doRun(suite(), false);
    if (result.wasSuccessful()) {
      System.exit(0);
    } else {
      System.exit(1);
    }
  }

  public MasterTester(String name) {
    super(name);
  }

  public static Test suite() {
    Logger.setupLogs (Logger.INFO);

    TestSuite result = new TestSuite();
    // This is possibly not right; the JIT needs to be disabled in order
    // for these tests to succeed.
    result.addTest(new TestSuite(TestUtilMDE.class));

    // To determine what should be in this list:
    //   find . -name '*Test*.java' | perl -pe 's:^.*/::' | grep -v MasterTester | sort

    result.addTest(new TestSuite(daikon.test.config.ConfigurationTest.class));
    result.addTest(new TestSuite
      (daikon.test.diff.ConsequentCVFSortComparatorTester.class));
    result.addTest(new TestSuite
      (daikon.test.diff.ConsequentCVFPairComparatorTester.class));
    result.addTest(new TestSuite
     (daikon.test.diff.DetailedStatisticsVisitorTester.class));
    result.addTest(new TestSuite(daikon.test.diff.DiffTester.class));
    result.addTest(new TestSuite(daikon.test.inv.InvariantTester.class));
    result.addTest(new TestSuite(daikon.test.diff.InvMapTester.class));
    result.addTest(new TestSuite(daikon.test.LinearTernaryCoreTest.class));
    result.addTest(new TestSuite(daikon.test.diff.MinusVisitorTester.class));
    result.addTest(new TestSuite
     (daikon.test.diff.PrintDifferingInvariantsVisitorTester.class));
    result.addTest(new TestSuite(daikon.test.ProglangTypeTest.class));
    result.addTest(new TestSuite(daikon.test.diff.UnionVisitorTester.class));
    result.addTest(new TestSuite(daikon.test.VarComparabilityTest.class));
    result.addTest(new TestSuite(daikon.test.VarInfoNameTest.class));
    result.addTest(new TestSuite(daikon.test.diff.XorVisitorTester.class));
    return result;
  }

}
