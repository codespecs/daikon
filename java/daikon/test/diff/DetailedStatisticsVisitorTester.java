package daikon.test.diff;

import junit.framework.*;
import daikon.*;
import daikon.inv.*;
import daikon.diff.*;

public class DetailedStatisticsVisitorTester extends TestCase {

  RootNode root = new RootNode();
  DetailedStatisticsVisitor v = new DetailedStatisticsVisitor(false);
  
  VarInfo[] vars = { DiffTester.newIntVarInfo("x"),
                     DiffTester.newIntVarInfo("y"),
                     DiffTester.newIntVarInfo("z") };
  PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
  
  PptSlice slice0 = ppt.implication_view;
  Invariant null_int_1_just = new DummyInvariant(slice0, "1", true);
  Invariant null_int_1_unjust = new DummyInvariant(slice0, "1", false);
  Invariant null_int_2_just = new DummyInvariant(slice0, "2", true);
  Invariant null_int_2_unjust = new DummyInvariant(slice0, "2", false);

  Invariant null_unint_1_just =
    new DummyInvariant(slice0, "1", true, false);
  Invariant null_unint_1_unjust =
    new DummyInvariant(slice0, "1", false, false);
  Invariant null_unint_2_just =
    new DummyInvariant(slice0, "2", true, false);
  Invariant null_unint_2_unjust =
    new DummyInvariant(slice0, "2", false, false);

  PptSlice slice1 = new PptSlice1(ppt, new VarInfo[] {vars[0]});
  Invariant unary_int_1_just = new DummyInvariant(slice1, "1", true);
  Invariant unary_int_1_unjust = new DummyInvariant(slice1, "1", false);
  Invariant unary_int_2_just = new DummyInvariant(slice1, "2", true);
  Invariant unary_int_2_unjust = new DummyInvariant(slice1, "2", false);

  Invariant unary_unint_1_just =
    new DummyInvariant(slice1, "1", true, false);
  Invariant unary_unint_1_unjust =
    new DummyInvariant(slice1, "1", false, false);
  Invariant unary_unint_2_just =
    new DummyInvariant(slice1, "2", true, false);
  Invariant unary_unint_2_unjust =
    new DummyInvariant(slice1, "2", false, false);

  PptSlice slice2 = new PptSlice2(ppt, new VarInfo[] {vars[0], vars[1]});
  Invariant binary_1_just = new DummyInvariant(slice2, "1", true);
  Invariant binary_1_unjust = new DummyInvariant(slice2, "1", false);
  Invariant binary_2_just = new DummyInvariant(slice2, "2", true);
  Invariant binary_2_unjust = new DummyInvariant(slice2, "2", false);
  
  PptSlice slice3 = new PptSlice3(ppt, vars);
  Invariant ternary_1_just = new DummyInvariant(slice3, "1", true);
  Invariant ternary_1_unjust = new DummyInvariant(slice3, "1", false);
  Invariant ternary_2_just = new DummyInvariant(slice3, "2", true);
  Invariant ternary_2_unjust = new DummyInvariant(slice3, "2", false);

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(DiffTester.class));
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
      for (int rel = 0;
           rel < DetailedStatisticsVisitor.NUM_RELATIONSHIPS;
           rel++) {
        Assert.assert(1 == v.freq(type, rel));
      }
    }
  }
}
