package daikon.test.diff;

import daikon.*;
import daikon.diff.*;
import daikon.inv.*;
import daikon.test.*;
import java.lang.reflect.*;
import junit.framework.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

public class DetailedStatisticsVisitorTester extends TestCase {

  RootNode root = new RootNode();
  DetailedStatisticsVisitor v = new DetailedStatisticsVisitor(false);

  VarInfo[] vars = {
    DiffTester.newIntVarInfo("x"), DiffTester.newIntVarInfo("y"), DiffTester.newIntVarInfo("z")
  };
  PptTopLevel ppt = Common.makePptTopLevel("Foo:::OBJECT", vars);

  PptSlice slice0 = ppt.joiner_view;
  Invariant null_int_1_just = new DiffDummyInvariant(slice0, "1", true);
  Invariant null_int_1_unjust = new DiffDummyInvariant(slice0, "1", false);
  Invariant null_int_2_just = new DiffDummyInvariant(slice0, "2", true);
  Invariant null_int_2_unjust = new DiffDummyInvariant(slice0, "2", false);

  Invariant null_unint_1_just = new DiffDummyInvariant(slice0, "1", true, false);
  Invariant null_unint_1_unjust = new DiffDummyInvariant(slice0, "1", false, false);
  Invariant null_unint_2_just = new DiffDummyInvariant(slice0, "2", true, false);
  Invariant null_unint_2_unjust = new DiffDummyInvariant(slice0, "2", false, false);

  Invariant null_noprint = new DiffDummyInvariant(slice0, "0", true, true, false);

  PptSlice slice1 = new PptSlice1(ppt, new VarInfo[] {vars[0]});
  Invariant unary_int_1_just = new DiffDummyInvariant(slice1, "1", true);
  Invariant unary_int_1_unjust = new DiffDummyInvariant(slice1, "1", false);
  Invariant unary_int_2_just = new DiffDummyInvariant(slice1, "2", true);
  Invariant unary_int_2_unjust = new DiffDummyInvariant(slice1, "2", false);

  Invariant unary_unint_1_just = new DiffDummyInvariant(slice1, "1", true, false);
  Invariant unary_unint_1_unjust = new DiffDummyInvariant(slice1, "1", false, false);
  Invariant unary_unint_2_just = new DiffDummyInvariant(slice1, "2", true, false);
  Invariant unary_unint_2_unjust = new DiffDummyInvariant(slice1, "2", false, false);

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

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(DetailedStatisticsVisitorTester.class));
  }

  public DetailedStatisticsVisitorTester(String name) {
    super(name);

    PptNode pptNode = new PptNode(ppt, ppt);

    pptNode.add(new InvNode(null_int_1_just, null_int_1_just));
    pptNode.add(new InvNode(null_int_1_just, null_int_1_unjust));
    pptNode.add(new InvNode(null_int_1_unjust, null_int_1_just));
    pptNode.add(new InvNode(null_int_1_unjust, null_int_1_unjust));
    pptNode.add(new InvNode(null_int_1_just, null_int_2_just));
    pptNode.add(new InvNode(null_int_1_just, null_int_2_unjust));
    pptNode.add(new InvNode(null_int_1_unjust, null_int_2_just));
    pptNode.add(new InvNode(null_int_1_unjust, null_int_2_unjust));
    pptNode.add(new InvNode(null_int_1_just, null));
    pptNode.add(new InvNode(null_int_1_unjust, null));
    pptNode.add(new InvNode(null, null_int_1_just));
    pptNode.add(new InvNode(null, null_int_1_unjust));

    pptNode.add(new InvNode(null_unint_1_just, null_unint_1_just));
    pptNode.add(new InvNode(null_unint_1_just, null_unint_1_unjust));
    pptNode.add(new InvNode(null_unint_1_unjust, null_unint_1_just));
    pptNode.add(new InvNode(null_unint_1_unjust, null_unint_1_unjust));
    pptNode.add(new InvNode(null_unint_1_just, null_unint_2_just));
    pptNode.add(new InvNode(null_unint_1_just, null_unint_2_unjust));
    pptNode.add(new InvNode(null_unint_1_unjust, null_unint_2_just));
    pptNode.add(new InvNode(null_unint_1_unjust, null_unint_2_unjust));
    pptNode.add(new InvNode(null_unint_1_just, null));
    pptNode.add(new InvNode(null_unint_1_unjust, null));
    pptNode.add(new InvNode(null, null_unint_1_just));
    pptNode.add(new InvNode(null, null_unint_1_unjust));

    pptNode.add(new InvNode(null_noprint, null_noprint));

    pptNode.add(new InvNode(unary_int_1_just, unary_int_1_just));
    pptNode.add(new InvNode(unary_int_1_just, unary_int_1_unjust));
    pptNode.add(new InvNode(unary_int_1_unjust, unary_int_1_just));
    pptNode.add(new InvNode(unary_int_1_unjust, unary_int_1_unjust));
    pptNode.add(new InvNode(unary_int_1_just, unary_int_2_just));
    pptNode.add(new InvNode(unary_int_1_just, unary_int_2_unjust));
    pptNode.add(new InvNode(unary_int_1_unjust, unary_int_2_just));
    pptNode.add(new InvNode(unary_int_1_unjust, unary_int_2_unjust));
    pptNode.add(new InvNode(unary_int_1_just, null));
    pptNode.add(new InvNode(unary_int_1_unjust, null));
    pptNode.add(new InvNode(null, unary_int_1_just));
    pptNode.add(new InvNode(null, unary_int_1_unjust));

    pptNode.add(new InvNode(unary_unint_1_just, unary_unint_1_just));
    pptNode.add(new InvNode(unary_unint_1_just, unary_unint_1_unjust));
    pptNode.add(new InvNode(unary_unint_1_unjust, unary_unint_1_just));
    pptNode.add(new InvNode(unary_unint_1_unjust, unary_unint_1_unjust));
    pptNode.add(new InvNode(unary_unint_1_just, unary_unint_2_just));
    pptNode.add(new InvNode(unary_unint_1_just, unary_unint_2_unjust));
    pptNode.add(new InvNode(unary_unint_1_unjust, unary_unint_2_just));
    pptNode.add(new InvNode(unary_unint_1_unjust, unary_unint_2_unjust));
    pptNode.add(new InvNode(unary_unint_1_just, null));
    pptNode.add(new InvNode(unary_unint_1_unjust, null));
    pptNode.add(new InvNode(null, unary_unint_1_just));
    pptNode.add(new InvNode(null, unary_unint_1_unjust));

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

  public void testFreq() {
    for (int type = 0; type < DetailedStatisticsVisitor.NUM_TYPES; type++) {
      for (int rel = 0; rel < DetailedStatisticsVisitor.NUM_RELATIONSHIPS; rel++) {
        assert 1 == (int) v.freq(type, rel);
      }
    }
  }

  public void testShouldAddFrequency() throws Exception {
    // Invoke private method using reflection
    Method m =
        DetailedStatisticsVisitor.class.getDeclaredMethod(
            "shouldAddFrequency", new Class<?>[] {Invariant.class, Invariant.class});
    m.setAccessible(true);

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    /*@NonNull*/ Boolean b1 = (Boolean) m.invoke(null, new Object[] {null_noprint, null_noprint});
    assert !b1.booleanValue();

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    /*@NonNull*/ Boolean b2 =
        (Boolean) m.invoke(null, new Object[] {null_int_1_just, null_int_1_just});
    assert b2.booleanValue();

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    /*@NonNull*/ Boolean b3 =
        (Boolean) m.invoke(null, new /*@Nullable*/ Object[] {null, null_noprint});
    assert !b3.booleanValue();

    @SuppressWarnings("nullness") // static method, so null first arg is OK: shouldAddFrequency()
    /*@NonNull*/ Boolean b4 =
        (Boolean) m.invoke(null, new /*@Nullable*/ Object[] {null, null_int_1_just});
    assert b4.booleanValue();
  }
}
