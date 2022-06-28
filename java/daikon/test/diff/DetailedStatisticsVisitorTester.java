package daikon.test.diff;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import daikon.test.*;
import java.lang.reflect.Method;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class DetailedStatisticsVisitorTester {

  RootNode root = new RootNode();
  DetailedStatisticsVisitor v = new DetailedStatisticsVisitor(false);

  VarInfo[] vars = {
    DiffTester.newIntVarInfo("x"), DiffTester.newIntVarInfo("y"), DiffTester.newIntVarInfo("z")
  };
  PptTopLevel ppt = Common.makePptTopLevel("Foo:::OBJECT", vars);

  PptSlice slice0 = ppt.joiner_view;
  Invariant null_1_just = new DiffDummyInvariant(slice0, "1", true);
  Invariant null_1_unjust = new DiffDummyInvariant(slice0, "1", false);
  Invariant null_2_just = new DiffDummyInvariant(slice0, "2", true);
  Invariant null_2_unjust = new DiffDummyInvariant(slice0, "2", false);

  Invariant null_noprint = new DiffDummyInvariant(slice0, "0", true, false);

  PptSlice slice1 = new PptSlice1(ppt, new VarInfo[] {vars[0]});
  Invariant unary_1_just = new DiffDummyInvariant(slice1, "1", true);
  Invariant unary_1_unjust = new DiffDummyInvariant(slice1, "1", false);
  Invariant unary_2_just = new DiffDummyInvariant(slice1, "2", true);
  Invariant unary_2_unjust = new DiffDummyInvariant(slice1, "2", false);

  PptSlice slice2 = new PptSlice2(ppt, new VarInfo[] {vars[0], vars[1]});
  Invariant binary_1_just = new DiffDummyInvariant(slice2, "1", true);
  Invariant binary_1_unjust = new DiffDummyInvariant(slice2, "1", false);
  Invariant binary_2_just = new DiffDummyInvariant(slice2, "2", true);
  Invariant binary_2_unjust = new DiffDummyInvariant(slice2, "2", false);

  PptSlice slice3 = new PptSlice3(ppt, vars);
  Invariant ternary_1_just = new DiffDummyInvariant(slice3, "1", true);
  Invariant ternary_1_unjust = new DiffDummyInvariant(slice3, "1", false);
  Invariant ternary_2_just = new DiffDummyInvariant(slice3, "2", true);
  Invariant ternary_2_unjust = new DiffDummyInvariant(slice3, "2", false);

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @Before
  public void setUp() {
    PptNode pptNode = new PptNode(ppt, ppt);

    pptNode.add(new InvNode(null_1_just, null_1_just));
    pptNode.add(new InvNode(null_1_just, null_1_unjust));
    pptNode.add(new InvNode(null_1_unjust, null_1_just));
    pptNode.add(new InvNode(null_1_unjust, null_1_unjust));
    pptNode.add(new InvNode(null_1_just, null_2_just));
    pptNode.add(new InvNode(null_1_just, null_2_unjust));
    pptNode.add(new InvNode(null_1_unjust, null_2_just));
    pptNode.add(new InvNode(null_1_unjust, null_2_unjust));
    pptNode.add(new InvNode(null_1_just, null));
    pptNode.add(new InvNode(null_1_unjust, null));
    pptNode.add(new InvNode(null, null_1_just));
    pptNode.add(new InvNode(null, null_1_unjust));

    pptNode.add(new InvNode(null_noprint, null_noprint));

    pptNode.add(new InvNode(unary_1_just, unary_1_just));
    pptNode.add(new InvNode(unary_1_just, unary_1_unjust));
    pptNode.add(new InvNode(unary_1_unjust, unary_1_just));
    pptNode.add(new InvNode(unary_1_unjust, unary_1_unjust));
    pptNode.add(new InvNode(unary_1_just, unary_2_just));
    pptNode.add(new InvNode(unary_1_just, unary_2_unjust));
    pptNode.add(new InvNode(unary_1_unjust, unary_2_just));
    pptNode.add(new InvNode(unary_1_unjust, unary_2_unjust));
    pptNode.add(new InvNode(unary_1_just, null));
    pptNode.add(new InvNode(unary_1_unjust, null));
    pptNode.add(new InvNode(null, unary_1_just));
    pptNode.add(new InvNode(null, unary_1_unjust));

    pptNode.add(new InvNode(binary_1_just, binary_1_just));
    pptNode.add(new InvNode(binary_1_just, binary_1_unjust));
    pptNode.add(new InvNode(binary_1_unjust, binary_1_just));
    pptNode.add(new InvNode(binary_1_unjust, binary_1_unjust));
    pptNode.add(new InvNode(binary_1_just, binary_2_just));
    pptNode.add(new InvNode(binary_1_just, binary_2_unjust));
    pptNode.add(new InvNode(binary_1_unjust, binary_2_just));
    pptNode.add(new InvNode(binary_1_unjust, binary_2_unjust));
    pptNode.add(new InvNode(binary_1_just, null));
    pptNode.add(new InvNode(binary_1_unjust, null));
    pptNode.add(new InvNode(null, binary_1_just));
    pptNode.add(new InvNode(null, binary_1_unjust));

    pptNode.add(new InvNode(ternary_1_just, ternary_1_just));
    pptNode.add(new InvNode(ternary_1_just, ternary_1_unjust));
    pptNode.add(new InvNode(ternary_1_unjust, ternary_1_just));
    pptNode.add(new InvNode(ternary_1_unjust, ternary_1_unjust));
    pptNode.add(new InvNode(ternary_1_just, ternary_2_just));
    pptNode.add(new InvNode(ternary_1_just, ternary_2_unjust));
    pptNode.add(new InvNode(ternary_1_unjust, ternary_2_just));
    pptNode.add(new InvNode(ternary_1_unjust, ternary_2_unjust));
    pptNode.add(new InvNode(ternary_1_just, null));
    pptNode.add(new InvNode(ternary_1_unjust, null));
    pptNode.add(new InvNode(null, ternary_1_just));
    pptNode.add(new InvNode(null, ternary_1_unjust));

    root.add(pptNode);

    root.accept(v);
  }

  /** Validate that this class's constructor added exactly one of each arity and relationship. */
  @Test
  public void testFreq() {
    for (int arity = 0; arity < DetailedStatisticsVisitor.NUM_ARITIES; arity++) {
      for (int rel = 0; rel < DetailedStatisticsVisitor.NUM_RELATIONSHIPS; rel++) {
        assertEquals(1, (int) v.freq(arity, rel));
      }
    }
  }

  @Test
  public void testShouldAddFrequency() throws Exception {
    // Invoke private method using reflection
    Method m =
        DetailedStatisticsVisitor.class.getDeclaredMethod(
            "shouldAddFrequency", new Class<?>[] {Invariant.class, Invariant.class});
    m.setAccessible(true);

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    @NonNull Boolean b1 = (Boolean) m.invoke(null, new Object[] {null_noprint, null_noprint});
    assertFalse(b1.booleanValue());

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    @NonNull Boolean b2 = (Boolean) m.invoke(null, new Object[] {null_1_just, null_1_just});
    assertTrue(b2.booleanValue());

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    @NonNull Boolean b3 = (Boolean) m.invoke(null, new @Nullable Object[] {null, null_noprint});
    assertFalse(b3.booleanValue());

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    @NonNull Boolean b4 = (Boolean) m.invoke(null, new @Nullable Object[] {null, null_1_just});
    assertTrue(b4.booleanValue());
  }
}
