package daikon.test;

import junit.framework.*;
import daikon.test.diff.*;
import daikon.test.inv.*;


public class MasterTester extends TestCase {

  public static void main(String[] args) {
    junit.textui.TestRunner.run(suite());
  }

  public MasterTester(String name) {
    super(name);
  }

  public static Test suite() {
    TestSuite result = new TestSuite();
    result.addTest(new TestSuite(DiffTester.class));
    result.addTest(new TestSuite(InvariantTester.class));
    result.addTest(new TestSuite(LinearTernaryCoreTest.class));
    result.addTest(new TestSuite(VarInfoNameTest.class));
    result.addTest(new TestSuite(ProglangTypeTester.class));
    return result;
  }

}
