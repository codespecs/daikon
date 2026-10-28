package daikon.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/** All Daikon's unit tests. Does not include system tests. */
@RunWith(Suite.class)
@Suite.SuiteClasses({
  daikon.chicory.RuntimeTest.class,
  daikon.test.BadOptionTest.class,
  daikon.test.config.ConfigurationTest.class,
  daikon.test.config.HtmlToTexinfoTest.class,
  daikon.test.DeclReaderTest.class,
  daikon.test.diff.ConsequentCVFPairComparatorTester.class,
  daikon.test.diff.ConsequentCVFSortComparatorTester.class,
  daikon.test.diff.DetailedStatisticsVisitorTester.class,
  daikon.test.diff.DiffTester.class,
  daikon.test.diff.InvMapTester.class,
  daikon.test.diff.MinusVisitorTester.class,
  daikon.test.diff.PrintDifferingInvariantsVisitorTester.class,
  daikon.test.diff.UnionVisitorTester.class,
  daikon.test.diff.XorVisitorTester.class,
  daikon.test.DtraceDiffTester.class,
  daikon.test.DtraceNonceFixerTest.class,
  daikon.test.InvariantFormatTester.class,
  daikon.test.inv.InvariantAddAndCheckTester.class,
  daikon.test.inv.InvariantTester.class,
  daikon.test.inv.unary.scalar.OneOfScalarTester.class,
  daikon.test.inv.unary.sequence.OneOfSequenceTester.class,
  daikon.test.LinearTernaryCoreTest.class,
  daikon.test.ModBitTrackerTest.class,
  daikon.test.PptDeclarationTest.class,
  daikon.test.PptTopLevelTypeTest.class,
  daikon.test.ProglangTypeTest.class,
  daikon.test.SampleTester.class,
  daikon.test.split.FileCompilerTest.class,
  daikon.test.split.NullComparisonsSplitterTest.class,
  daikon.test.split.QualifiedNamesSplitterTest.class,
  daikon.test.split.SplitterDuplicatesTest.class,
  daikon.test.split.SplitterFactoryFileNameTest.class,
  daikon.test.split.SplitterListTest.class,
  daikon.test.split.SplitterLoadTest.class,
  daikon.test.TestAnnotate.class,
  daikon.test.TestAst.class,
  daikon.test.TestClassOrInterfaceTypeDecorateVisitor.class,
  daikon.test.TestQuant.class,
  daikon.test.TraceSelectTest.class,
  daikon.test.VarComparabilityTest.class,
  daikon.test.VarDefinitionBoundTest.class,
  daikon.test.VarInfoAuxTest.class,
  daikon.test.VarInfoNameTest.class,
  // daikon.test.split.SplitterFactoryTest is not in this suite, because it is generated after the
  // rest of Daikon (including this class) is compiled.  The Makefile's "junit" target runs it
  // separately.
})
public final class AllTestsSuite {}
