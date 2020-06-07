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
    Class<? extends TestCase>[] classes =
        (Class<? extends TestCase>[])
            new Class[] {
              daikon.test.TestClassOrInterfaceTypeDecorateVisitor.class,
              daikon.test.TestAst.class,
              daikon.test.config.ConfigurationTest.class,
              daikon.test.config.HtmlToTexinfoTest.class,
              daikon.test.diff.ConsequentCVFPairComparatorTester.class,
              daikon.test.diff.ConsequentCVFSortComparatorTester.class,
              daikon.test.diff.DetailedStatisticsVisitorTester.class,
              daikon.test.diff.DiffTester.class,
              daikon.test.diff.InvMapTester.class,
              daikon.test.diff.MinusVisitorTester.class,
              daikon.test.diff.PrintDifferingInvariantsVisitorTester.class,
              daikon.test.diff.UnionVisitorTester.class,
              daikon.test.diff.XorVisitorTester.class,
              daikon.test.InvariantFormatTester.class,
              daikon.test.SampleTester.class,
              daikon.test.inv.InvariantTester.class,
              daikon.test.inv.unary.scalar.OneOfScalarTester.class,
              daikon.test.inv.unary.sequence.OneOfSequenceTester.class,
              daikon.test.LinearTernaryCoreTest.class,
              daikon.test.ModBitTrackerTest.class,
              daikon.test.ProglangTypeTest.class,
              daikon.test.VarComparabilityTest.class,
              daikon.test.VarInfoNameTest.class,
              daikon.test.inv.InvariantAddAndCheckTester.class,
              daikon.test.TestQuant.class,
              daikon.test.TestAnnotate.class,
              daikon.test.DtraceDiffTester.class,

              //       ,
              //       // I'm having trouble with this; need to fix, reinstate, and not call
              //       // specially from Makefile.  -MDE 7/8/2005
              //       daikon.test.split.SplitterFactoryTest.class
            };

    for (int i = 0; i < classes.length; i++) {
      result.addTest(new TestSuite(classes[i]));
    }

    return result;
  }
}
