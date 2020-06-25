package daikon.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/** All Daikon's unit tests. Does not include system tests. */
@RunWith(Suite.class)
@Suite.SuiteClasses({
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
})
public final class AllTestsSuite {}
