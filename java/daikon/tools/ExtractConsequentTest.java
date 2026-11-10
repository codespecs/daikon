package daikon.tools;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.FileIO.VarDefinition;
import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptSlice2;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarInfo;
import daikon.VarInfo.VarKind;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoScalar.IntGreaterThan;
import daikon.inv.binary.twoScalar.IntNonEqual;
import daikon.inv.unary.scalar.NonZero;
import daikon.inv.unary.scalar.OneOfFloat;
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.test.Common;
import java.util.Arrays;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link ExtractConsequent}. */
@RunWith(JUnit4.class)
public class ExtractConsequentTest {

  /** Prepares for tests. */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
  }

  /**
   * Returns a new boolean variable, whose file representation type is boolean.
   *
   * @param name the name of the variable
   * @return a new boolean variable
   */
  @SuppressWarnings("interning") // newly-created VarInfo
  private static VarInfo newBooleanVarInfo(String name) {
    return new VarInfo(new VarDefinition(name, VarKind.VARIABLE, ProglangType.BOOLEAN));
  }

  @Test
  public void testParenthesizeIfNeeded() {
    assertEquals("x > 0", ExtractConsequent.parenthesizeIfNeeded("x > 0"));
    assertEquals("x >= y", ExtractConsequent.parenthesizeIfNeeded("x >= y"));
    assertEquals("x <= y", ExtractConsequent.parenthesizeIfNeeded("x <= y"));
    assertEquals("x == y && y != z", ExtractConsequent.parenthesizeIfNeeded("x == y && y != z"));
    assertEquals("(x > 0 || y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 || y > 0"));
    assertEquals("(x > 0 or y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 or y > 0"));
    assertEquals("(x > 0 ==> y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 ==> y > 0"));
    assertEquals("(x > 0 <==> y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 <==> y > 0"));
    assertEquals("(b ? x : y)", ExtractConsequent.parenthesizeIfNeeded("b ? x : y"));
    assertEquals("(x > 0 <== y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 <== y > 0"));
    assertEquals(
        "(x > 0 <=!=> y > 0)", ExtractConsequent.parenthesizeIfNeeded("x > 0 <=!=> y > 0"));

    // Operators within literals are not operators.
    assertEquals("s.equals(\"a?b\")", ExtractConsequent.parenthesizeIfNeeded("s.equals(\"a?b\")"));
    assertEquals(
        "s == \"this or \\\" that\"",
        ExtractConsequent.parenthesizeIfNeeded("s == \"this or \\\" that\""));
    assertEquals("c == '?'", ExtractConsequent.parenthesizeIfNeeded("c == '?'"));
    assertEquals(
        "(c == '?' || s == \"||\")",
        ExtractConsequent.parenthesizeIfNeeded("c == '?' || s == \"||\""));
  }

  @Test
  public void testConjunction() {
    assertEquals(
        "(x > 0 || y > 0) && z > 0",
        ExtractConsequent.conjunction(Arrays.asList("x > 0 || y > 0", "z > 0"), " && "));
    assertEquals(
        "x > 0 and (y > 0 or z > 0)",
        ExtractConsequent.conjunction(Arrays.asList("x > 0", "y > 0 or z > 0"), " and "));
    assertEquals(
        "x > 0 && s == \"||\"",
        ExtractConsequent.conjunction(Arrays.asList("x > 0", "s == \"||\""), " && "));
  }

  @SuppressWarnings("nullness:argument") // JUnit annotations forbid null actual values
  @Test
  public void testToSpinfoCondition() {
    assertEquals("return > 0", ExtractConsequent.toSpinfoCondition("\\result > 0"));
    assertEquals(
        "return.x == return.y", ExtractConsequent.toSpinfoCondition("\\result.x == \\result.y"));
    assertEquals("\\resultx > 0", ExtractConsequent.toSpinfoCondition("\\resultx > 0"));
    assertEquals(
        "s.equals(\"\\\\result\")",
        ExtractConsequent.toSpinfoCondition("s.equals(\"\\\\result\")"));
    assertEquals(
        "return.equals(\"\\\\result\") && return != null",
        ExtractConsequent.toSpinfoCondition("\\result.equals(\"\\\\result\") && \\result != null"));

    assertEquals("orig(x) > 0", ExtractConsequent.toSpinfoCondition("\\old(x) > 0"));
    assertEquals(
        "return == orig(this.size)",
        ExtractConsequent.toSpinfoCondition("\\result == \\old(this.size)"));
    assertEquals(
        "s.equals(\"\\\\old(x)\")",
        ExtractConsequent.toSpinfoCondition("s.equals(\"\\\\old(x)\")"));
  }

  @Test
  public void testPostPattern() {
    assertTrue(ExtractConsequent.post_pattern.matcher("\\post(x) > 0").find());
    assertTrue(ExtractConsequent.post_pattern.matcher("x == \\post (y)").find());
    assertTrue(
        ExtractConsequent.post_pattern
            .matcher("orig(daikon.Quant.getElement_int(a, \\new(i))) > 0")
            .find());
    assertTrue(ExtractConsequent.post_pattern.matcher("x == \\new (y)").find());
    assertFalse(ExtractConsequent.post_pattern.matcher("new Foo() == x").find());
    assertFalse(ExtractConsequent.post_pattern.matcher("x > 0").find());
    assertFalse(ExtractConsequent.post_pattern.matcher("orig(x) > 0").find());
    assertFalse(ExtractConsequent.post_pattern.matcher("\\old(x) > 0").find());
    assertFalse(ExtractConsequent.post_pattern.matcher("this.post(x)").find());
  }

  @SuppressWarnings("nullness:argument") // JUnit annotations forbid null actual values
  @Test
  public void testToSpinfoConditionUnusable() {
    assertNull(ExtractConsequent.toSpinfoCondition("\\post(x) > 0"));
    assertNull(ExtractConsequent.toSpinfoCondition("x.getClass() == Foo.class"));
    assertNull(ExtractConsequent.toSpinfoCondition("\\old(a[\\new(i)]) > 0"));
    assertNull(ExtractConsequent.toSpinfoCondition("s.equals(\"\\\\post(x)\") && \\post(y) > 0"));
    assertEquals("x > 0", ExtractConsequent.toSpinfoCondition("x > 0"));
    assertEquals("this.classCount > 0", ExtractConsequent.toSpinfoCondition("this.classCount > 0"));
    assertEquals(
        "this.classifier == null", ExtractConsequent.toSpinfoCondition("this.classifier == null"));
    assertEquals("Foo.class$0 == null", ExtractConsequent.toSpinfoCondition("Foo.class$0 == null"));
    assertEquals(
        "this.class\u00e9 > 0", ExtractConsequent.toSpinfoCondition("this.class\u00e9 > 0"));
    assertEquals(
        "s.equals(\"\\\\post(x)\")",
        ExtractConsequent.toSpinfoCondition("s.equals(\"\\\\post(x)\")"));
    assertEquals(
        "s.equals(\"\\\\new(x)\")",
        ExtractConsequent.toSpinfoCondition("s.equals(\"\\\\new(x)\")"));
    assertEquals(
        "s.equals(\"Foo.class\")", ExtractConsequent.toSpinfoCondition("s.equals(\"Foo.class\")"));
  }

  @SuppressWarnings("interning") // newly-created VarInfo
  @Test
  public void testIsContradictory() {
    VarInfo x = Common.newIntVarInfo("x");
    VarInfo y = Common.newIntVarInfo("y");
    VarInfo b = newBooleanVarInfo("b");
    VarInfo d = new VarInfo(new VarDefinition("d", VarKind.VARIABLE, ProglangType.DOUBLE));

    // "x == 1 && x == 2", "b == true && b == false", and "d == 0.5 && d == 1.5" are always false.
    assertTrue(ExtractConsequent.isContradictory(Arrays.asList(oneOf(x, 1), oneOf(x, 2))));
    assertTrue(
        ExtractConsequent.isContradictory(Arrays.asList(oneOf(b, 1), oneOf(y, 3), oneOf(b, 0))));
    assertTrue(
        ExtractConsequent.isContradictory(Arrays.asList(oneOfFloat(d, 0.5), oneOfFloat(d, 1.5))));

    // "x == 1 && y == 2", "x == 1 && x == 1", and "x == 1 && x one of { 1, 2 }" can be true.
    assertFalse(ExtractConsequent.isContradictory(Arrays.asList(oneOf(x, 1), oneOf(y, 2))));
    assertFalse(ExtractConsequent.isContradictory(Arrays.asList(oneOf(x, 1), oneOf(x, 1))));
    assertFalse(ExtractConsequent.isContradictory(Arrays.asList(oneOf(x, 1), oneOf(x, 1, 2))));
    // Floating-point values that are approximately equal are equal.
    assertFalse(
        ExtractConsequent.isContradictory(
            Arrays.asList(oneOfFloat(d, 0.5), oneOfFloat(d, 0.5 + 1e-12))));
  }

  /**
   * Instantiates an invariant, failing the test if the invariant cannot be instantiated.
   *
   * @param proto the prototype of the invariant
   * @param slice the slice over whose variables to instantiate the invariant
   * @return the instantiated invariant
   */
  private static Invariant instantiate(Invariant proto, PptSlice slice) {
    Invariant result = proto.instantiate(slice);
    if (result == null) {
      throw new AssertionError("Cannot instantiate " + proto.getClass() + " over " + slice);
    }
    return result;
  }

  /**
   * Returns an invariant over two variables.
   *
   * @param proto the prototype of the invariant
   * @param v1 the first variable
   * @param v2 the second variable
   * @return an invariant over the two variables
   */
  private static Invariant binaryInvariant(Invariant proto, VarInfo v1, VarInfo v2) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::ENTER", new VarInfo[] {v1, v2});
    return instantiate(proto, new PptSlice2(ppt, new VarInfo[] {v1, v2}));
  }

  /**
   * Returns an IntNonEqual invariant over two variables, even if a variable is a boolean.
   *
   * @param v1 a variable
   * @param v2 another variable
   * @return an IntNonEqual invariant over the two variables
   */
  private static Invariant intNonEqual(VarInfo v1, VarInfo v2) {
    // By default, Daikon does not instantiate IntNonEqual over a boolean.
    boolean saved = IntNonEqual.dkconfig_integral_only;
    IntNonEqual.dkconfig_integral_only = false;
    try {
      return binaryInvariant(IntNonEqual.get_proto(), v1, v2);
    } finally {
      IntNonEqual.dkconfig_integral_only = saved;
    }
  }

  @Test
  public void testIsLegalForBooleans() {
    VarInfo b1 = newBooleanVarInfo("b1");
    VarInfo b2 = newBooleanVarInfo("b2");
    VarInfo i1 = Common.newIntVarInfo("i1");
    VarInfo i2 = Common.newIntVarInfo("i2");

    // "b1 == b2", "b1 != b2", "i1 == i2", and "i1 > i2" are legal Java.
    assertTrue(ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), b1, b2)));
    assertTrue(ExtractConsequent.isLegalForBooleans(intNonEqual(b1, b2)));
    assertTrue(ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), i1, i2)));
    assertTrue(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntGreaterThan.get_proto(), i1, i2)));

    // "b1 == i1" and "b1 != i1" are not legal Java.  (Daikon does not instantiate an ordering
    // invariant such as IntGreaterThan over a boolean, so "b1 > b2" cannot arise.)
    assertFalse(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), b1, i1)));
    assertFalse(ExtractConsequent.isLegalForBooleans(intNonEqual(b1, i1)));

    // "b1 != 0" is not legal Java.
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::ENTER", new VarInfo[] {b1});
    assertFalse(
        ExtractConsequent.isLegalForBooleans(
            instantiate(NonZero.get_proto(), new PptSlice1(ppt, new VarInfo[] {b1}))));

    // OneOfScalar formats a boolean as "b1 == true" or "b1 == false".
    assertTrue(ExtractConsequent.isLegalForBooleans(oneOf(b1)));
    assertTrue(ExtractConsequent.isLegalForBooleans(oneOf(b1, 1)));
    assertTrue(ExtractConsequent.isLegalForBooleans(oneOf(b1, 0)));
    assertTrue(ExtractConsequent.isLegalForBooleans(oneOf(i1)));
  }

  /**
   * Returns a OneOfScalar invariant over the given variable, which has the given values.
   *
   * @param var a variable
   * @param values the values of the variable
   * @return a OneOfScalar invariant over {@code var}
   */
  private static Invariant oneOf(VarInfo var, long... values) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::ENTER", new VarInfo[] {var});
    OneOfScalar result =
        (OneOfScalar) instantiate(OneOfScalar.get_proto(), new PptSlice1(ppt, new VarInfo[] {var}));
    for (long value : values) {
      result.add_modified(value, 1);
    }
    return result;
  }

  /**
   * Returns a OneOfFloat invariant over the given variable, which has the given values.
   *
   * @param var a floating-point variable
   * @param values the values of the variable
   * @return a OneOfFloat invariant over {@code var}
   */
  private static Invariant oneOfFloat(VarInfo var, double... values) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(double):::ENTER", new VarInfo[] {var});
    OneOfFloat result =
        (OneOfFloat) instantiate(OneOfFloat.get_proto(), new PptSlice1(ppt, new VarInfo[] {var}));
    for (double value : values) {
      result.add_modified(value, 1);
    }
    return result;
  }

  /**
   * Returns the cluster key of the invariant "var == value".
   *
   * @param var a variable
   * @param value the value of the variable
   * @return the cluster key of the invariant "var == value"
   */
  private static String clusterKey(VarInfo var, long value) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::EXIT", new VarInfo[] {var});
    OneOfScalar inv =
        (OneOfScalar) instantiate(OneOfScalar.get_proto(), new PptSlice1(ppt, new VarInfo[] {var}));
    inv.add_modified(value, 1);
    return ExtractConsequent.clusterKey(inv);
  }

  @SuppressWarnings("interning") // newly-created VarInfo
  @Test
  public void testClusterKey() {
    VarInfo cluster = new VarInfo(new VarDefinition("cluster", VarKind.VARIABLE, ProglangType.INT));
    VarInfo origCluster = VarInfo.origVarInfo(cluster);
    // Daikon.create_orig_vars sets postState.
    origCluster.postState = cluster;
    assertTrue(origCluster.includes_simple_name("cluster"));

    assertEquals(clusterKey(cluster, 1), clusterKey(origCluster, 1));
    // Normalization does not conflate different clusters.
    assertFalse(clusterKey(origCluster, 1).equals(clusterKey(origCluster, 2)));
    // Normalization rewrites "orig(cluster)" in an invariant over another variable as well.
    VarInfo x = Common.newIntVarInfo("x");
    assertEquals(
        "cluster > x",
        ExtractConsequent.clusterKey(binaryInvariant(IntGreaterThan.get_proto(), origCluster, x)));
    // An invariant over both the pre-state and the post-state value is its own key.
    Invariant unchanged = binaryInvariant(IntEqual.get_proto(), cluster, origCluster);
    assertEquals("cluster == orig(cluster)", ExtractConsequent.clusterKey(unchanged));
  }
}
