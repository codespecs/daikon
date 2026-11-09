package daikon.test.inv.unary.scalar;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.FileIO.VarDefinition;
import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarComparabilityNone;
import daikon.VarInfo;
import daikon.VarInfo.VarKind;
import daikon.VarInfoAux;
import daikon.inv.InvariantStatus;
import daikon.inv.OutputFormat;
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.test.Common;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.junit.BeforeClass;
import org.junit.Test;
import org.plumelib.util.Intern;

@SuppressWarnings("nullness") // testing code
public class OneOfScalarTester {

  private VarInfo[] vars = {Common.makeHashcodeVarInfo("x"), Common.newIntVarInfo("y")};
  private PptTopLevel ppt = Common.makePptTopLevel("Foo.Baa(int):::ENTER", vars);
  private PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
  // private PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});

  private static final int DOESNT_MATTER = 0;

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
  }

  @SuppressWarnings("interning")
  public static VarInfo newIntVarInfo(String name) {
    VarInfo result =
        new VarInfo(
            name,
            ProglangType.INT,
            ProglangType.INT,
            VarComparabilityNone.it,
            VarInfoAux.getDefault());
    return result;
  }

  @SuppressWarnings("interning")
  public static VarInfo newHashcodeVarInfo(String name) {
    VarInfo result =
        new VarInfo(
            name,
            ProglangType.HASHCODE,
            ProglangType.HASHCODE,
            VarComparabilityNone.it,
            VarInfoAux.getDefault());
    return result;
  }

  @Test
  public void testNullNon() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assertFalse(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNullNull() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNonNon() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  /**
   * Returns a OneOfScalar invariant over the given variable, which has the given value.
   *
   * @param var a variable
   * @param value the value of the variable
   * @return a OneOfScalar invariant over {@code var}
   */
  private static OneOfScalar oneOf(VarInfo var, long value) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int):::ENTER", new VarInfo[] {var});
    OneOfScalar result =
        (OneOfScalar) OneOfScalar.get_proto().instantiate(new PptSlice1(ppt, new VarInfo[] {var}));
    result.add_modified(value, DOESNT_MATTER);
    return result;
  }

  @SuppressWarnings("interning") // newly-created VarInfo
  @Test
  public void testIntRepresentedBoolean() {
    // A boolean variable whose file representation type is int.
    VarDefinition bDef = new VarDefinition("b", VarKind.VARIABLE, ProglangType.BOOLEAN);
    bDef.rep_type = ProglangType.INT;
    bDef.useBooleanRepType();
    VarInfo b = new VarInfo(bDef);
    assertEquals(ProglangType.BOOLEAN, b.file_rep_type);
    OneOfScalar bTrue = oneOf(b, 1);
    assertTrue(bTrue.is_boolean());
    assertEquals("b == true", bTrue.format_using(OutputFormat.JAVA));
    assertEquals("b == true", bTrue.format_using(OutputFormat.DAIKON));
    assertEquals("b == true", bTrue.format_using(OutputFormat.ESCJAVA));
    assertEquals("(EQ |b| |@true|)", bTrue.format_using(OutputFormat.SIMPLIFY));
    assertEquals("b == false", oneOf(b, 0).format_using(OutputFormat.JAVA));

    // As for a boolean whose file representation type is boolean, a second value falsifies the
    // invariant, so Daikon never infers "b one of { 0, 1 }".
    assertEquals(InvariantStatus.FALSIFIED, oneOf(b, 0).add_modified(1, DOESNT_MATTER));

    // An int variable is not a boolean.
    VarInfo i = new VarInfo(new VarDefinition("i", VarKind.VARIABLE, ProglangType.INT));
    OneOfScalar iOne = oneOf(i, 1);
    assertFalse(iOne.is_boolean());
    assertEquals("i == 1", iOne.format_using(OutputFormat.JAVA));
  }

  @Test
  public void testUseBooleanRepType() {
    // A boolean whose rep type is integral gets rep type boolean.
    VarDefinition bDef = new VarDefinition("b", VarKind.VARIABLE, ProglangType.BOOLEAN);
    bDef.rep_type = ProglangType.LONG_PRIMITIVE;
    bDef.useBooleanRepType();
    assertEquals(ProglangType.BOOLEAN, bDef.rep_type);

    // Such a variable loses its bounds, and a constant value other than 0 or 1 becomes 1.
    VarDefinition cDef = new VarDefinition("c", VarKind.VARIABLE, ProglangType.BOOLEAN);
    cDef.rep_type = ProglangType.INT;
    cDef.min_value = "0";
    cDef.max_value = "1";
    cDef.static_constant_value = Intern.internedLong(2);
    cDef.useBooleanRepType();
    assertEquals(ProglangType.BOOLEAN, cDef.rep_type);
    assertNull(cDef.min_value);
    assertNull(cDef.max_value);
    assertEquals(Long.valueOf(1), cDef.static_constant_value);
    @SuppressWarnings("interning") // newly-created VarInfo
    VarInfo c = new VarInfo(cDef); // checkRep succeeds
    assertEquals(ProglangType.BOOLEAN, c.file_rep_type);

    // A boolean whose rep type is not integral keeps its rep type.
    VarDefinition hDef = new VarDefinition("h", VarKind.VARIABLE, ProglangType.BOOLEAN);
    hDef.rep_type = ProglangType.HASHCODE;
    hDef.useBooleanRepType();
    assertEquals(ProglangType.HASHCODE, hDef.rep_type);

    // A variable that is not declared as a boolean keeps its rep type.
    VarDefinition iDef = new VarDefinition("i", VarKind.VARIABLE, ProglangType.INT);
    iDef.useBooleanRepType();
    assertEquals(ProglangType.INT, iDef.rep_type);
  }

  /* NEED TO DEFINE SEMANTICS WITH MIKE E
  @Test
  public void testNullNonHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assertFalse( inv1.isSameFormula(inv2));
  }

  @Test
  public void testNullNullHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNonNonHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }
  */
}
