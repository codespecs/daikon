package daikon.test;

import junit.framework.*;
import daikon.test.diff.*;
import daikon.test.inv.*;
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

    result.addTest(new TestSuite(DiffTester.class));
    result.addTest(new TestSuite(DetailedStatisticsVisitorTester.class));
    result.addTest(new TestSuite(InvariantTester.class));
    result.addTest(new TestSuite(LinearTernaryCoreTest.class));
    result.addTest(new TestSuite(ProglangTypeTest.class));
    result.addTest(new TestSuite(VarComparabilityTest.class));
    result.addTest(new TestSuite(VarInfoNameTest.class));
    return result;
  }

}
