package daikon.test.diff;

import junit.framework.*;
import daikon.*;
import daikon.inv.*;
import daikon.diff.*;
import java.lang.reflect.*;

public class PrintDifferingInvariantsVisitorTester extends TestCase {

  VarInfo[] vars = { DiffTester.newIntVarInfo("x"),
                     DiffTester.newIntVarInfo("y"),
                     DiffTester.newIntVarInfo("z") };
  PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);

  PptSlice slice0 = ppt.implication_view;
  Invariant null_int_1_just = new DummyInvariant(slice0, "1", true);
  Invariant null_noprint = new DummyInvariant(slice0, "0", true, true, false);
  Invariant null_uninteresting = new DummyInvariant(slice0, "0", true, false, true);

  public static void main(String[] args) {
    daikon.Logger.setupLogs (daikon.Logger.INFO);
    junit.textui.TestRunner.run(new TestSuite(DiffTester.class));
  }

  public PrintDifferingInvariantsVisitorTester(String name) {
    super(name);
  }

  public void testShouldPrint() throws Exception {
    // Invoke private method using reflection
    Method m = PrintDifferingInvariantsVisitor.class.getDeclaredMethod
      ("shouldPrint", new Class[] {Invariant.class, Invariant.class});
    m.setAccessible(true);

    PrintDifferingInvariantsVisitor v =
      new PrintDifferingInvariantsVisitor(null, false, false, false);

    Boolean b = (Boolean) m.invoke
      (v, new Object[] {null_noprint, null_noprint});
    Assert.assertTrue(!b.booleanValue());

    // Test printing of uninteresting invariants
    b = (Boolean) m.invoke
      (v, new Object[] {null_uninteresting, null_uninteresting});
    Assert.assertTrue(!b.booleanValue());
    PrintDifferingInvariantsVisitor vu =
      new PrintDifferingInvariantsVisitor(null, false, false, true);
    b = (Boolean) m.invoke
      (vu, new Object[] {null_uninteresting, null_uninteresting});
    Assert.assertTrue(b.booleanValue());

    b = (Boolean) m.invoke
      (v, new Object[] {null_int_1_just, null_noprint});
    Assert.assertTrue(b.booleanValue());

    b = (Boolean) m.invoke
      (v, new Object[] {null, null_noprint});
    Assert.assertTrue(!b.booleanValue());

    b = (Boolean) m.invoke
      (v, new Object[] {null, null_int_1_just});
    Assert.assertTrue(b.booleanValue());
  }

}
