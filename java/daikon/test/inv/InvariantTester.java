package daikon.test.inv;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.*;
import daikon.config.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;
import daikon.test.*;
import java.util.Comparator;
import java.util.Random;
import junit.framework.*;
import org.junit.BeforeClass;
import org.junit.Test;

/** Daikon unit test class. */
@SuppressWarnings("nullness") // testing code
public class InvariantTester {

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  @SuppressWarnings("interning")
  public VarInfo newIntVarInfo(String name) {
    return new VarInfo(
        name, ProglangType.INT, ProglangType.INT, VarComparabilityNone.it, VarInfoAux.getDefault());
  }

  @Test
  public void testClassVarnameComparator() {
    Comparator<Invariant> c = new Invariant.ClassVarnameComparator();

    VarInfo[] vars = {Common.newIntVarInfo("x"), Common.newIntVarInfo("y")};
    PptTopLevel ppt = Common.makePptTopLevel("Foo:::OBJECT", vars);
    PptSlice slice = new PptSlice2(ppt, vars);

    Invariant inv1, inv2, inv2_2, inv2_3, inv2_4, inv2_5, inv2_6, inv3, inv4, inv5, inv6;

    Configuration.getInstance()
        .apply("daikon.inv.binary.twoScalar.NumericInt.BitwiseComplement.enabled = 1");

    inv1 = NumericInt.BitwiseComplement.get_proto().instantiate(slice);
    assertEquals(0, c.compare(inv1, inv1));

    inv2 = IntEqual.get_proto().instantiate(slice);
    inv2_2 = IntNonEqual.get_proto().instantiate(slice);
    inv2_3 = IntLessThan.get_proto().instantiate(slice);
    inv2_4 = IntLessEqual.get_proto().instantiate(slice);
    inv2_5 = IntGreaterThan.get_proto().instantiate(slice);
    inv2_6 = IntGreaterEqual.get_proto().instantiate(slice);
    assertTrue(c.compare(inv1, inv2) > 0);
    assertTrue(c.compare(inv1, inv2_2) > 0);
    assertTrue(c.compare(inv1, inv2_3) > 0);
    assertTrue(c.compare(inv1, inv2_4) > 0);
    assertTrue(c.compare(inv1, inv2_5) > 0);
    assertTrue(c.compare(inv1, inv2_6) > 0);

    inv3 = LinearBinary.get_proto().instantiate(slice);
    assertTrue(c.compare(inv3, inv1) < 0);

    inv4 = IntNonEqual.get_proto().instantiate(slice);
    assertTrue(c.compare(inv1, inv4) > 0);

    inv5 = Implication.makeImplication(ppt, inv1, inv2, false, inv1, inv2);
    inv6 = Implication.makeImplication(ppt, inv1, inv3, false, inv1, inv3);
    assertTrue(c.compare(inv5, inv6) < 0);

    inv5 = Implication.makeImplication(ppt, inv2, inv1, false, inv2, inv1);
    inv6 = Implication.makeImplication(ppt, inv2, inv3, false, inv2, inv3);
    assertTrue(c.compare(inv5, inv6) > 0);

    inv5 = Implication.makeImplication(ppt, inv3, inv2, false, inv3, inv2);
    inv6 = Implication.makeImplication(ppt, inv3, inv1, false, inv3, inv1);
    assertTrue(c.compare(inv5, inv6) < 0);

    inv5 = Implication.makeImplication(ppt, inv1, inv4, false, inv1, inv4);
    inv6 = Implication.makeImplication(ppt, inv3, inv4, false, inv3, inv4);
    assertTrue(c.compare(inv5, inv6) > 0);

    inv5 = Implication.makeImplication(ppt, inv2, inv4, false, inv2, inv4);
    inv6 = Implication.makeImplication(ppt, inv4, inv1, false, inv4, inv1);
    assertTrue(c.compare(inv5, inv6) < 0);

    VarInfo[] vars2 = {Common.newIntVarInfo("x"), Common.newIntVarInfo("z")};
    PptTopLevel ppt2 = Common.makePptTopLevel("Foo:::OBJECT", vars2);
    PptSlice slice2 = new PptSlice2(ppt2, vars2);
    inv2 = NumericInt.BitwiseComplement.get_proto().instantiate(slice2);
    assertTrue(c.compare(inv1, inv2) < 0);

    vars2[0] = Common.newIntVarInfo("a");
    vars2[1] = Common.newIntVarInfo("y");
    ppt2 = Common.makePptTopLevel("Foo:::OBJECT", vars2);
    slice2 = new PptSlice2(ppt2, vars2);
    inv2 = NumericInt.BitwiseComplement.get_proto().instantiate(slice2);
    assertTrue(c.compare(inv1, inv2) > 0);
  }

  @Test
  public void test_prob_is_ge() {
    assertEquals(1.0, Invariant.prob_is_ge(0, 11), 0);
    assertEquals(1.0, Invariant.prob_is_ge(1, 11), 0);
    assertEquals(.9, Invariant.prob_is_ge(2, 11), 0);
    assertEquals(.8, Invariant.prob_is_ge(3, 11), 0);
    assertEquals(.2, Invariant.prob_is_ge(9, 11), 0);
    assertEquals(.1, Invariant.prob_is_ge(10, 11), 0);
    assertEquals(0.0, Invariant.prob_is_ge(11, 11), 0);
    assertEquals(0.0, Invariant.prob_is_ge(20, 11), 0);
  }

  @Test
  public void test_prob_and() {

    assertEquals(0.0, Invariant.prob_and(0, 0), 0);
    assertEquals(1.0, Invariant.prob_and(0, 1), 0);
    assertEquals(1.0, Invariant.prob_and(1, 0), 0);
    assertEquals(1.0, Invariant.prob_and(1, 1), 0);
    assertEquals(.5, Invariant.prob_and(0, .5), 0);
    assertEquals(.5, Invariant.prob_and(.5, 0), 0);
    assertEquals(1.0, Invariant.prob_and(1, .5), 0);
    assertEquals(1.0, Invariant.prob_and(.5, 1), 0);
    assertEquals(.1, Invariant.prob_and(0, .1), 0);
    assertEquals(.1, Invariant.prob_and(.1, 0), 0);
    assertEquals(1.0, Invariant.prob_and(1, .1), 0);
    assertEquals(1.0, Invariant.prob_and(.1, 1), 0);
    assertEquals(.75, Invariant.prob_and(.5, .5), 0);
    assertEquals(.91, Invariant.prob_and(.1, .9), 0);
    assertEquals(.91, Invariant.prob_and(.9, .1), 0);

    Random r = new Random(20010907);
    for (int i = 0; i < 100; i++) {
      double x = r.nextDouble();
      double y = r.nextDouble();
      double z = r.nextDouble();
      double r1 = Invariant.prob_and(x, y, z);
      double r2 = Invariant.prob_and(x, Invariant.prob_and(y, z));
      double r3 = Invariant.prob_and(Invariant.prob_and(x, y), z);
      assertEquals(r1, r2, .000001);
      assertEquals(r1, r3, .000001);
      assertEquals(r2, r3, .000001);
    }
  }

  @Test
  public void test_prob_or() {
    assertEquals(0.0, Invariant.prob_or(0, 0), 0);
    assertEquals(0.0, Invariant.prob_or(0, 1), 0);
    assertEquals(0.0, Invariant.prob_or(1, 0), 0);
    assertEquals(1.0, Invariant.prob_or(1, 1), 0);
    assertEquals(0.0, Invariant.prob_or(0, .5), 0);
    assertEquals(0.0, Invariant.prob_or(.5, 0), 0);
    assertEquals(.5, Invariant.prob_or(1, .5), 0);
    assertEquals(.5, Invariant.prob_or(.5, 1), 0);
    assertEquals(0.0, Invariant.prob_or(0, .1), 0);
    assertEquals(0.0, Invariant.prob_or(.1, 0), 0);
    assertEquals(.1, Invariant.prob_or(1, .1), 0);
    assertEquals(.1, Invariant.prob_or(.1, 1), 0);
    assertEquals(.5, Invariant.prob_or(.5, .5), 0);
    assertEquals(.1, Invariant.prob_or(.1, .9), 0);
    assertEquals(.1, Invariant.prob_or(.9, .1), 0);
  }
}
