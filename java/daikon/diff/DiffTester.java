package daikon.diff;

import junit.framework.*;
import daikon.*;
import daikon.inv.*;
import daikon.inv.unary.scalar.*;
import daikon.inv.unary.string.*;
import daikon.inv.unary.sequence.*;
import daikon.inv.unary.stringsequence.*;
import daikon.inv.binary.twoScalar.*;
import daikon.inv.ternary.threeScalar.*;
import utilMDE.Pair;

import java.util.*;
import java.io.*;
import javax.swing.tree.*;

public class DiffTester extends TestCase {

  private PptMap empty;
  private PptMap ppts1;
  private PptMap ppts2;
  private PptMap ppts3;
  private PptMap invs1;
  private PptMap invs2;
  private PptMap invs3;
  
  
  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(DiffTester.class));
  }

  public DiffTester(String name) {
    super(name);
    
    empty = new PptMap();

    ppts1 = new PptMap();
    ppts1.add(new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ppts1.add(new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ppts1.add(new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]));

    ppts2 = new PptMap();
    ppts2.add(new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ppts2.add(new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));

    // Permutation of ppts1
    ppts3 = new PptMap();
    ppts3.add(new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ppts3.add(new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ppts3.add(new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]));
    
    {
      invs1 = new PptMap();
      VarInfo[] vars = {
        new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
      PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
      PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
      Invariant invx = LowerBound.instantiate(slicex);
      slicex.addInvariant(invx);
      PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
      Invariant invy = LowerBound.instantiate(slicey);
      slicey.addInvariant(invy);
      PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
      Invariant invz = LowerBound.instantiate(slicez);
      slicez.addInvariant(invz);
      Vector v = new Vector();
      v.add(slicex);
      v.add(slicey);
      v.add(slicez);
      ppt.addViews(v);
      invs1.add(ppt);
    }

    {
      // Permutation of invs1
      invs2 = new PptMap();
      VarInfo[] vars = {
        new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
      PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
      PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
      Invariant invy = LowerBound.instantiate(slicey);
      slicey.addInvariant(invy);
      PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
      Invariant invx = LowerBound.instantiate(slicex);
      slicex.addInvariant(invx);
      PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
      Invariant invz = LowerBound.instantiate(slicez);
      slicez.addInvariant(invz);
      Vector v = new Vector();
      v.add(slicey);
      v.add(slicex);
      v.add(slicez);
      ppt.addViews(v);
      invs2.add(ppt);
    }

    {
      invs3 = new PptMap();
      VarInfo[] vars = {
        new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
        new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
      PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
      PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
      Invariant invx = LowerBound.instantiate(slicex);
      slicex.addInvariant(invx);
      PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
      Invariant invy = UpperBound.instantiate(slicey);
      slicex.addInvariant(invy);
      PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
      Invariant invz = LowerBound.instantiate(slicez);
      slicez.addInvariant(invz);
      Vector v = new Vector();
      v.add(slicex);
      v.add(slicey);
      v.add(slicez);
      ppt.addViews(v);
      invs3.add(ppt);
    }


  }

  public void testEmptyEmpty() {
    RootNode diff = Diff.diffPptMap(empty, empty);
    RootNode ref = new RootNode();
    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testEmptyPpts1() {
    RootNode diff = Diff.diffPptMap(empty, ppts1);

    RootNode ref = new RootNode();
    PptNode node;
    node = new PptNode
      (null,
       new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (null,
       new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (null,
       new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]));
    ref.add(node);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testPpts1Empty() {
    RootNode diff = Diff.diffPptMap(ppts1, empty);

    RootNode ref = new RootNode();
    PptNode node;
    node = new PptNode
      (new PptTopLevel("Foo:::OBJECT", new VarInfo[0]),
       null);
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]),
       null);
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]),
       null);
    ref.add(node);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }


  public void testPpts1Ppts1() {
    RootNode diff = Diff.diffPptMap(ppts1, ppts1);

    RootNode ref = new RootNode();
    PptNode node;
    node = new PptNode
      (new PptTopLevel("Foo:::OBJECT", new VarInfo[0]),
       new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]),
       new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]),
       new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]));
    ref.add(node);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testPpts1Ppts2() {
    RootNode diff = Diff.diffPptMap(ppts1, ppts2);

    RootNode ref = new RootNode();
    PptNode node;
    node = new PptNode
      (new PptTopLevel("Foo:::OBJECT", new VarInfo[0]),
       new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]),
       new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]),
       null);
    ref.add(node);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testPpts1Ppts3() {
    RootNode diff = Diff.diffPptMap(ppts1, ppts3);

    RootNode ref = new RootNode();
    PptNode node;
    node = new PptNode
      (new PptTopLevel("Foo:::OBJECT", new VarInfo[0]),
       new PptTopLevel("Foo:::OBJECT", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]),
       new PptTopLevel("Foo.Bar(int):::ENTER", new VarInfo[0]));
    ref.add(node);
    node = new PptNode
      (new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]),
       new PptTopLevel("Foo.Bar(int):::EXIT19", new VarInfo[0]));
    ref.add(node);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }


  public void testInvs1Empty() {
    RootNode diff = Diff.diffPptMap(invs1, empty);

    RootNode ref = new RootNode();

    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
    Invariant invx = LowerBound.instantiate(slicex);
    slicex.addInvariant(invx);
    PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
    Invariant invy = LowerBound.instantiate(slicey);
    slicey.addInvariant(invy);
    PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
    Invariant invz = LowerBound.instantiate(slicez);
    slicez.addInvariant(invz);

    PptNode pptNode;
    pptNode = new PptNode(ppt, null);
    InvNode invNode;
    invNode = new InvNode(invx, null);
    pptNode.add(invNode);
    invNode = new InvNode(invy, null);
    pptNode.add(invNode);
    invNode = new InvNode(invz, null);
    pptNode.add(invNode);
    ref.add(pptNode);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(3, countDifferingInvariants(diff));
  }

  public void testInvs1Invs1() {
    RootNode diff = Diff.diffPptMap(invs1, invs1);

    RootNode ref = new RootNode();

    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
    Invariant invx = LowerBound.instantiate(slicex);
    slicex.addInvariant(invx);
    PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
    Invariant invy = LowerBound.instantiate(slicey);
    slicey.addInvariant(invy);
    PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
    Invariant invz = LowerBound.instantiate(slicez);
    slicez.addInvariant(invz);

    PptNode pptNode;
    pptNode = new PptNode(ppt, ppt);
    InvNode invNode;
    invNode = new InvNode(invx, invx);
    pptNode.add(invNode);
    invNode = new InvNode(invy, invy);
    pptNode.add(invNode);
    invNode = new InvNode(invz, invz);
    pptNode.add(invNode);
    ref.add(pptNode);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testInvs1Invs2() {
    RootNode diff = Diff.diffPptMap(invs1, invs2);

    RootNode ref = new RootNode();

    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
    Invariant invx = LowerBound.instantiate(slicex);
    slicex.addInvariant(invx);
    PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
    Invariant invy = LowerBound.instantiate(slicey);
    slicey.addInvariant(invy);
    PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
    Invariant invz = LowerBound.instantiate(slicez);
    slicez.addInvariant(invz);

    PptNode pptNode;
    pptNode = new PptNode(ppt, ppt);
    InvNode invNode;
    invNode = new InvNode(invx, invx);
    pptNode.add(invNode);
    invNode = new InvNode(invy, invy);
    pptNode.add(invNode);
    invNode = new InvNode(invz, invz);
    pptNode.add(invNode);
    ref.add(pptNode);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testInvs1Invs3() {
    RootNode diff = Diff.diffPptMap(invs1, invs3);

    RootNode ref = new RootNode();

    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
    Invariant invx = LowerBound.instantiate(slicex);
    PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});
    Invariant invy1 = LowerBound.instantiate(slicey);
    Invariant invy2 = UpperBound.instantiate(slicey);
    PptSlice slicez = new PptSlice1(ppt, new VarInfo[] {vars[2]});
    Invariant invz = LowerBound.instantiate(slicez);

    PptNode pptNode;
    pptNode = new PptNode(ppt, ppt);
    InvNode invNode;
    invNode = new InvNode(invx, invx);
    pptNode.add(invNode);
    invNode = new InvNode(invy1, null);
    pptNode.add(invNode);
    invNode = new InvNode(invz, invz);
    pptNode.add(invNode);
    invNode = new InvNode(null, invy2);
    pptNode.add(invNode);
    ref.add(pptNode);

    Assert.assertEquals(printTree(ref), printTree(diff));
    Assert.assertEquals(2, countDifferingInvariants(diff));
  }

  public void testNonModulus() {
    PptMap map = new PptMap();
    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slice = new PptSlice1(ppt, vars);
    Invariant inv = NonModulus.instantiate(slice);
    slice.addInvariant(inv);
    Vector v = new Vector();
    v.add(slice);
    map.add(ppt);

    RootNode diff = Diff.diffPptMap(map, map);
    Assert.assertEquals(0, countDifferingInvariants(diff));
  }

  public void testStatisticsVisitor() {
    RootNode root = new RootNode();

    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};

    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);

    PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
    LowerBound invxLower1 = LowerBound.instantiate(slicex);
    LowerBound invxLower2 = LowerBound.instantiate(slicex);
    invxLower2.core.min1 = 19;
    UpperBound invxUpper1 = UpperBound.instantiate(slicex);
    UpperBound invxUpper2 = UpperBound.instantiate(slicex);
    invxUpper2.core.max1 = 19;
    OneOfScalar invxOneOfScalar1 = OneOfScalar.instantiate(slicex);
    OneOfScalar invxOneOfScalar2 = OneOfScalar.instantiate(slicex);
    invxOneOfScalar2.add_modified(19, 0);
    EltLowerBound invxEltLower1 = EltLowerBound.instantiate(slicex);
    EltLowerBound invxEltLower2 = EltLowerBound.instantiate(slicex);
    invxEltLower2.core.min1 = 19;
    
    PptSlice slicexy = new PptSlice2(ppt, new VarInfo[] {vars[0], vars[1]});
    IntComparison invxyIntComparison1 = IntComparison.instantiate(slicexy);
    IntComparison invxyIntComparison2 = IntComparison.instantiate(slicexy);
    invxyIntComparison2.core.can_be_eq = true;
    
    PptSlice slicexyz = new PptSlice3(ppt, vars);
    FunctionBinary invxyzFunctionBinary1 =
      FunctionBinary.instantiate(slicexyz, "foo", null, 0);
    FunctionBinary invxyzFunctionBinary2 =
      FunctionBinary.instantiate(slicexyz, "bar", null, 0);

    
    /*
    Invariant impl1 =
      Implication.makeImplication(ppt, invxLower, invxUpper, false);
    Invariant impl2 =
      Implication.makeImplication(ppt, invxLower, invxNonZero, false);
    */


    PptNode pptNode;
    pptNode = new PptNode(ppt, ppt);
    InvNode invNode;

    /*
    invNode = new InvNode(impl1, impl1);
    pptNode.add(invNode);
    invNode = new InvNode(impl2, impl2);
    pptNode.add(invNode);
    invNode = new InvNode(impl1, null);
    pptNode.add(invNode);    
    invNode = new InvNode(null, impl2);
    pptNode.add(invNode);    
    invNode = new InvNode(impl1, impl2);
    pptNode.add(invNode);    
    */

    invNode = new InvNode(invxLower1, invxLower1);
    pptNode.add(invNode);
    invNode = new InvNode(invxLower1, null);
    pptNode.add(invNode);
    invNode = new InvNode(null, invxLower1);
    pptNode.add(invNode);
    invNode = new InvNode(invxLower1, invxLower2);
    pptNode.add(invNode);
    invNode = new InvNode(invxUpper1, invxUpper2);
    pptNode.add(invNode);
    invNode = new InvNode(invxOneOfScalar1, invxOneOfScalar2);
    pptNode.add(invNode);
    invNode = new InvNode(invxEltLower1, invxEltLower2);
    pptNode.add(invNode);

    invNode = new InvNode(invxyIntComparison1, invxyIntComparison1);
    pptNode.add(invNode);
    invNode = new InvNode(invxyIntComparison1, null);
    pptNode.add(invNode);
    invNode = new InvNode(invxyIntComparison1, invxyIntComparison2);
    pptNode.add(invNode);

    invNode = new InvNode(invxyzFunctionBinary1, invxyzFunctionBinary1);
    pptNode.add(invNode);
    invNode = new InvNode(invxyzFunctionBinary1, null);
    pptNode.add(invNode);
    invNode = new InvNode(invxyzFunctionBinary1, invxyzFunctionBinary2);
    pptNode.add(invNode);


    root.add(pptNode);

    StatisticsVisitor v = new StatisticsVisitor();
    root.accept(v);
    System.out.println();
    System.out.println(v.format());

    Assert.assertEquals(0, v.getIdenticalNullary());
    Assert.assertEquals(0, v.getMissingNullary());
    Assert.assertEquals(0, v.getDifferingNullary());
    Assert.assertEquals(0, v.getDifferingInterestingNullary());
    Assert.assertEquals(0, v.getDifferingUninterestingNullary());

    Assert.assertEquals(1, v.getIdenticalUnary());
    Assert.assertEquals(2, v.getMissingUnary());
    Assert.assertEquals(4, v.getDifferingUnary());
    Assert.assertEquals(1, v.getDifferingInterestingUnary());
    Assert.assertEquals(3, v.getDifferingUninterestingUnary());

    Assert.assertEquals(1, v.getIdenticalBinary());
    Assert.assertEquals(1, v.getMissingBinary());
    Assert.assertEquals(1, v.getDifferingBinary());
    Assert.assertEquals(1, v.getDifferingInterestingBinary());
    Assert.assertEquals(0, v.getDifferingUninterestingBinary());

    Assert.assertEquals(1, v.getIdenticalTernary());
    Assert.assertEquals(1, v.getMissingTernary());
    Assert.assertEquals(1, v.getDifferingTernary());
    Assert.assertEquals(1, v.getDifferingInterestingTernary());
    Assert.assertEquals(0, v.getDifferingUninterestingTernary());    
  }

  private static String printTree(RootNode root) {
    PrintAllVisitor v = new PrintAllVisitor();
    root.accept(v);
    return v.getOutput();
  }

  private static String printDifferences(RootNode root) {
    PrintDifferingInvariantsVisitor v = new PrintDifferingInvariantsVisitor();
    root.accept(v);
    return v.getOutput();
  }

  private static int countDifferingInvariants(RootNode root) {
    CountDifferingInvariantsVisitor v = new CountDifferingInvariantsVisitor();
    root.accept(v);
    return v.getDifferingInvariantsCount();
  }



}
