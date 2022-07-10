package daikon.test.diff;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import daikon.inv.unary.scalar.*;
import daikon.test.*;
import java.util.Comparator;
import junit.framework.*;
import org.junit.BeforeClass;
import org.junit.Test;

/** Daikon unit test class. */
@SuppressWarnings("nullness") // testing code
public class ConsequentCVFPairComparatorTester {

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @Test
  public void testCompare() {
    VarInfo[] vars = {
      DiffTester.newIntVarInfo("a"),
      DiffTester.newIntVarInfo("b"),
      DiffTester.newIntVarInfo("c"),
      DiffTester.newIntVarInfo("d"),
    };
    PptTopLevel P = Common.makePptTopLevel("P:::OBJECT", vars);
    PptSlice slicea = new PptSlice1(P, new VarInfo[] {vars[0]});
    PptSlice sliceb = new PptSlice1(P, new VarInfo[] {vars[1]});
    PptSlice slicec = new PptSlice1(P, new VarInfo[] {vars[2]});
    PptSlice sliced = new PptSlice1(P, new VarInfo[] {vars[3]});
    Invariant A = NonZero.get_proto().instantiate(slicea);
    Invariant B = NonZero.get_proto().instantiate(sliceb);
    Invariant C = NonZero.get_proto().instantiate(slicec);
    Invariant D = NonZero.get_proto().instantiate(sliced);
    Invariant A_B = Implication.makeImplication(P, A, B, false, A, B);
    Invariant A_C = Implication.makeImplication(P, A, C, false, A, C);
    Invariant B_C = Implication.makeImplication(P, B, C, false, B, C);

    Comparator<Invariant> c = new ConsequentCVFPairComparator();

    assertEquals(0, c.compare(A, A));
    assertTrue(c.compare(A, B) < 0);
    assertTrue(c.compare(A, C) < 0);
    assertTrue(c.compare(A, D) < 0);
    assertTrue(c.compare(A, A_B) < 0);
    assertTrue(c.compare(A, A_C) < 0);
    assertTrue(c.compare(A, B_C) < 0);

    assertEquals(0, c.compare(B, B));
    assertTrue(c.compare(B, C) < 0);
    assertTrue(c.compare(B, D) < 0);
    assertEquals(0, c.compare(B, A_B));
    assertTrue(c.compare(B, A_C) < 0);
    assertTrue(c.compare(B, B_C) < 0);

    assertEquals(0, c.compare(C, C));
    assertTrue(c.compare(C, D) < 0);
    assertTrue(c.compare(C, A_B) > 0);
    assertEquals(0, c.compare(C, A_C));
    assertEquals(0, c.compare(C, B_C));

    assertEquals(0, c.compare(D, D));
    assertTrue(c.compare(D, A_B) > 0);
    assertTrue(c.compare(D, A_C) > 0);
    assertTrue(c.compare(D, B_C) > 0);

    // These are all < 0, because the LHS invariant is compared to the
    // RHS consequent.  The LHS has class Implication, while the RHS
    // has NonZero.  "Implication" < "NonZero"
    assertTrue(c.compare(A_B, A_B) < 0);
    assertTrue(c.compare(A_B, A_C) < 0);
    assertTrue(c.compare(A_B, B_C) < 0);

    assertTrue(c.compare(A_C, A_C) < 0);
    assertTrue(c.compare(A_C, B_C) < 0);

    assertTrue(c.compare(B_C, B_C) < 0);
  }
}
