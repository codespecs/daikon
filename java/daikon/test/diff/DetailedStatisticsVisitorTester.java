package daikon.test.diff;

import junit.framework.*;
import daikon.*;
import daikon.inv.*;
import daikon.diff.*;

public class DetailedStatisticsVisitorTester extends TestCase {

  RootNode root = new RootNode();
  DetailedStatisticsVisitor v = new DetailedStatisticsVisitor();
  
  VarInfo[] vars = { DiffTester.newIntVarInfo("x"),
                     DiffTester.newIntVarInfo("y"),
                     DiffTester.newIntVarInfo("z") };
  PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
  
  PptSlice slice0 = ppt.implication_view;
  Invariant null_1_just = new DummyInvariant(slice0, "1", true);
  Invariant null_1_unjust = new DummyInvariant(slice0, "1", false);
  Invariant null_2_just = new DummyInvariant(slice0, "2", true);
  Invariant null_2_unjust = new DummyInvariant(slice0, "2", false);

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(DiffTester.class));
  }

  public DetailedStatisticsVisitorTester(String name) {
    super(name);

    PptNode pptNode = new PptNode(ppt, ppt);
    InvNode invNode;
    
    invNode = new InvNode(null_1_just, null_1_just);
    pptNode.add(invNode);
    invNode = new InvNode(null_1_just, null_1_unjust);
    pptNode.add(invNode);

    root.add(pptNode);

    root.accept(v);
  }

  public void testFreq() {
    String result = v.format();
    //    System.out.println(result);
  }

}
