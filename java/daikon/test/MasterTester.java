package daikon.test;

import junit.framework.*;
import utilMDE.*;

public class MasterTester extends TestCase {

  public static void main(String[] args) {
    junit.textui.TestRunner.run(suite());
  }

  public MasterTester(String name) {
    super(name);
  }

  public static Test suite() {
    TestSuite result = new TestSuite();
    // This is possibly not right; the JIT needs to be disabled in order
    // for these tests to succeed.
    result.addTest(new TestSuite(TestUtilMDE.class));

    // To determine what should be in this list:
    //   find . -name '*Test*.java' | perl -pe 's:^.*/::' | grep -v MasterTester | sort

    result.addTest(new TestSuite(daikon.test.diff.DiffTester.class));
    result.addTest(new TestSuite(daikon.test.diff.DetailedStatisticsVisitorTester.class));
    result.addTest(new TestSuite(daikon.test.inv.InvariantTester.class));
    result.addTest(new TestSuite(daikon.test.LinearTernaryCoreTest.class));
    result.addTest(new TestSuite(daikon.test.ProglangTypeTest.class));
    result.addTest(new TestSuite(daikon.test.VarComparabilityTest.class));
    result.addTest(new TestSuite(daikon.test.VarInfoNameTest.class));
    result.addTest(new TestSuite(daikon.test.config.ConfigurationTest.class));
    return result;
  }

}
