package daikon.test.diff;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import java.lang.reflect.Method;
import junit.framework.*;
import org.junit.BeforeClass;
import org.junit.Test;

@SuppressWarnings("nullness") // testing code
public class PrintDifferingInvariantsVisitorTester {

  VarInfo[] vars = {
    DiffTester.newIntVarInfo("x"), DiffTester.newIntVarInfo("y"), DiffTester.newIntVarInfo("z")
  };

  /** The program point that contains the test invariants. */
  PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);

  /** The slice that contains the test invariants. */
  PptSlice slice0 = ppt.joiner_view;

  /** An invariant that is justified. */
  Invariant null_1_just = new DiffDummyInvariant(slice0, "1", true);

  /** An invariant that is justified but not worth printing. */
  Invariant null_noprint = new DiffDummyInvariant(slice0, "0", true, false);

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @Test
  public void testShouldPrint() throws Exception {
    // Invoke private method using reflection
    Method m =
        PrintDifferingInvariantsVisitor.class.getDeclaredMethod(
            "shouldPrint", new Class<?>[] {Invariant.class, Invariant.class});
    m.setAccessible(true);

    PrintDifferingInvariantsVisitor v = new PrintDifferingInvariantsVisitor(null, false, false);

    Boolean b = (Boolean) m.invoke(v, new Object[] {null_noprint, null_noprint});
    assertFalse(b.booleanValue());

    b = (Boolean) m.invoke(v, new Object[] {null_1_just, null_noprint});
    assertTrue(b.booleanValue());

    b = (Boolean) m.invoke(v, new Object[] {null, null_noprint});
    assertFalse(b.booleanValue());

    b = (Boolean) m.invoke(v, new Object[] {null, null_1_just});
    assertTrue(b.booleanValue());
  }
}
