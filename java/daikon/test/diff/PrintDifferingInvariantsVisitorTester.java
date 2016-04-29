package daikon.test.diff;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import java.lang.reflect.*;
import junit.framework.*;

@SuppressWarnings("nullness") // testing code
public class PrintDifferingInvariantsVisitorTester extends TestCase {

  VarInfo[] vars = {
    DiffTester.newIntVarInfo("x"), DiffTester.newIntVarInfo("y"), DiffTester.newIntVarInfo("z")
  };
  PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);

  PptSlice slice0 = ppt.joiner_view;
  Invariant null_int_1_just = new DiffDummyInvariant(slice0, "1", true);
  Invariant null_noprint = new DiffDummyInvariant(slice0, "0", true, true, false);
  Invariant null_uninteresting = new DiffDummyInvariant(slice0, "0", true, false, true);

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(DiffTester.class));
  }

  public PrintDifferingInvariantsVisitorTester(String name) {
    super(name);
  }

  public void testShouldPrint() throws Exception {
    // Invoke private method using reflection
    Method m =
        PrintDifferingInvariantsVisitor.class
            .getDeclaredMethod("shouldPrint", new Class<?>[] {Invariant.class, Invariant.class});
    m.setAccessible(true);

    PrintDifferingInvariantsVisitor v =
        new PrintDifferingInvariantsVisitor(null, false, false, false);

    Boolean b = (Boolean) m.invoke(v, new Object[] {null_noprint, null_noprint});
    assert !b.booleanValue();

    // Test printing of uninteresting invariants
    b = (Boolean) m.invoke(v, new Object[] {null_uninteresting, null_uninteresting});
    assert !b.booleanValue();
    PrintDifferingInvariantsVisitor vu =
        new PrintDifferingInvariantsVisitor(null, false, false, true);
    b = (Boolean) m.invoke(vu, new Object[] {null_uninteresting, null_uninteresting});
    assert b.booleanValue();

    b = (Boolean) m.invoke(v, new Object[] {null_int_1_just, null_noprint});
    assert b.booleanValue();

    b = (Boolean) m.invoke(v, new Object[] {null, null_noprint});
    assert !b.booleanValue();

    b = (Boolean) m.invoke(v, new Object[] {null, null_int_1_just});
    assert b.booleanValue();
  }
}
