package daikon.inv;

import junit.framework.*;
import daikon.*;
import daikon.inv.binary.twoScalar.*;
import java.util.*;

public class InvariantTester extends TestCase {

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(InvariantTester.class));
  }

  public InvariantTester(String name) {
    super(name);
  }

  public void testClassVarnameComparator() {
    Comparator c = new Invariant.ClassVarnameComparator();
    
    VarInfo[] vars = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt = new PptTopLevel("Foo:::OBJECT", vars);
    PptSlice slice = new PptSlice2(ppt, vars);

    Invariant inv1;
    Invariant inv2;
    
    inv1 = FunctionUnary.instantiate(slice, null, null, false);
    inv2 = IntComparison.instantiate(slice);
    Assert.assertTrue(c.compare(inv1, inv2) < 0);

    inv1 = LinearBinary.instantiate(slice);
    inv2 = IntComparison.instantiate(slice);
    Assert.assertTrue(c.compare(inv1, inv2) > 0);

    inv1 = IntComparison.instantiate(slice);
    inv2 = IntComparison.instantiate(slice);
    Assert.assertTrue(c.compare(inv1, inv2) == 0);

    VarInfo[] vars2 = {
      new VarInfo("x", "x", ProglangType.INT, ProglangType.INT, null),
      new VarInfo("z", "z", ProglangType.INT, ProglangType.INT, null)};
    PptTopLevel ppt2 = new PptTopLevel("Foo:::OBJECT", vars2);
    PptSlice slice2 = new PptSlice2(ppt2, vars2);
    inv2 = IntComparison.instantiate(slice2);
    Assert.assertTrue(c.compare(inv1, inv2) < 0);

    vars2[0] = new VarInfo("a", "a", ProglangType.INT, ProglangType.INT, null);
    vars2[1] = new VarInfo("y", "y", ProglangType.INT, ProglangType.INT, null);
    ppt2 = new PptTopLevel("Foo:::OBJECT", vars2);
    slice2 = new PptSlice2(ppt2, vars2);
    inv2 = IntComparison.instantiate(slice2);
    Assert.assertTrue(c.compare(inv1, inv2) > 0);
  }

}
