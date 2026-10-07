package daikon.tools;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.FileIO;
import daikon.FileIO.VarDefinition;
import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptSlice2;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarComparabilityNone;
import daikon.VarInfo;
import daikon.VarInfo.VarKind;
import daikon.VarInfoAux;
import daikon.inv.Invariant;
import daikon.inv.binary.twoScalar.IntEqual;
import daikon.inv.binary.twoScalar.IntGreaterThan;
import daikon.inv.binary.twoScalar.IntNonEqual;
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.test.Common;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

/** Tests for {@link ExtractConsequent}. */
@RunWith(JUnit4.class)
public class ExtractConsequentTest {

  /** The value of {@code FileIO.new_decl_format} before these tests ran. */
  private static @Nullable Boolean savedNewDeclFormat;

  /** Prepares for tests. */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    savedNewDeclFormat = FileIO.new_decl_format;
    FileIO.new_decl_format = true;
  }

  /** Restores global state, so that these tests do not affect other tests. */
  @AfterClass
  public static void tearDownClass() {
    if (savedNewDeclFormat == null) {
      FileIO.resetNewDeclFormat();
    } else {
      FileIO.new_decl_format = savedNewDeclFormat;
    }
  }

  /**
   * Returns a new boolean variable whose representation type is int. Daikon infers numeric
   * invariants, such as "b1 &gt; b2", over such a variable.
   *
   * @param name the name of the variable
   * @return a new boolean variable
   */
  @SuppressWarnings("interning") // newly-created VarInfo
  private static VarInfo newBooleanVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.BOOLEAN,
        ProglangType.INT,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
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
  public void testOrigPattern() {
    assertTrue(ExtractConsequent.orig_pattern.matcher("orig(x) > 0").find());
    assertTrue(ExtractConsequent.orig_pattern.matcher("x == orig (y)").find());
    assertTrue(ExtractConsequent.orig_pattern.matcher("\\old(x) > 0").find());
    assertTrue(ExtractConsequent.orig_pattern.matcher("\\new(x) > 0").find());
    assertFalse(ExtractConsequent.orig_pattern.matcher("x > 0").find());
    assertFalse(ExtractConsequent.orig_pattern.matcher("borig(x) > 0").find());
    assertFalse(ExtractConsequent.orig_pattern.matcher("this.isOrig(x)").find());
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

  @Test
  public void testIsLegalForBooleans() {
    VarInfo b1 = newBooleanVarInfo("b1");
    VarInfo b2 = newBooleanVarInfo("b2");
    VarInfo i1 = Common.newIntVarInfo("i1");
    VarInfo i2 = Common.newIntVarInfo("i2");

    // "b1 == b2", "b1 != b2", "i1 == i2", and "i1 > i2" are legal Java.
    assertTrue(ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), b1, b2)));
    assertTrue(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntNonEqual.get_proto(), b1, b2)));
    assertTrue(ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), i1, i2)));
    assertTrue(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntGreaterThan.get_proto(), i1, i2)));

    // "b1 == i1", "b1 != i1", and "b1 > b2" are not legal Java.
    assertFalse(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntEqual.get_proto(), b1, i1)));
    assertFalse(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntNonEqual.get_proto(), b1, i1)));
    assertFalse(
        ExtractConsequent.isLegalForBooleans(binaryInvariant(IntGreaterThan.get_proto(), b1, b2)));

    // OneOfScalar formats a boolean as "b1 == true" or "b1 == false".
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::ENTER", new VarInfo[] {b1});
    Invariant oneOf = instantiate(OneOfScalar.get_proto(), new PptSlice1(ppt, new VarInfo[] {b1}));
    assertTrue(ExtractConsequent.isLegalForBooleans(oneOf));
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
    assertFalse(clusterKey(cluster, 1).equals(clusterKey(origCluster, 2)));
  }
}
