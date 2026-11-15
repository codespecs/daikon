package daikon.test.inv.unary.sequence;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
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
import daikon.inv.unary.sequence.EltOneOf;
import daikon.inv.unary.sequence.OneOfSequence;
import daikon.test.Common;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.junit.BeforeClass;
import org.junit.Test;
import org.plumelib.util.Intern;

@SuppressWarnings("nullness") // testing code
public class OneOfSequenceTester {

  private VarInfo[] vars = {Common.makeHashcodeArrayVarInfo("x"), Common.makeIntArrayVarInfo("y")};
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
  public static VarInfo newIntArrayVarInfo(String name) {
    VarInfo result =
        new VarInfo(
            name,
            ProglangType.INT_ARRAY,
            ProglangType.INT_ARRAY,
            VarComparabilityNone.it,
            VarInfoAux.getDefault());
    return result;
  }

  @SuppressWarnings("interning")
  public static VarInfo newHashcodeArrayVarInfo(String name) {
    VarInfo result =
        new VarInfo(
            name,
            ProglangType.HASHCODE_ARRAY,
            ProglangType.HASHCODE_ARRAY,
            VarComparabilityNone.it,
            VarInfoAux.getDefault());
    return result;
  }

  /**
   * Returns an EltOneOf invariant over the given variable, which has the given value.
   *
   * @param var an array variable
   * @param value the value of the variable
   * @return an EltOneOf invariant over {@code var}
   */
  private static EltOneOf eltOneOf(VarInfo var, long... value) {
    PptTopLevel ppt = Common.makePptTopLevel("Foo.bar(int[]):::ENTER", new VarInfo[] {var});
    EltOneOf result =
        (EltOneOf) EltOneOf.get_proto().instantiate(new PptSlice1(ppt, new VarInfo[] {var}));
    result.add_modified(Intern.intern(value), DOESNT_MATTER);
    return result;
  }

  @SuppressWarnings("interning") // newly-created VarInfo
  @Test
  public void testIntRepresentedBooleanArray() {
    // A boolean[] variable whose file representation type is int[].
    VarDefinition aDef = new VarDefinition("a[]", VarKind.VARIABLE, ProglangType.BOOLEAN_ARRAY);
    aDef.rep_type = ProglangType.INT_ARRAY;
    aDef.arr_dims = 1;
    aDef.useBooleanRepType();
    assertEquals(ProglangType.BOOLEAN_ARRAY, aDef.rep_type);
    VarInfo a = new VarInfo(aDef);
    EltOneOf aTrue = eltOneOf(a, 1, 1);
    assertTrue(aTrue.is_boolean());
    assertEquals("a[] elements == true", aTrue.format_using(OutputFormat.DAIKON));

    // As for a boolean[] whose file representation type is boolean[], a second element value
    // falsifies the invariant, so Daikon never infers "a[] elements one of { 0, 1 }".
    assertEquals(
        InvariantStatus.FALSIFIED,
        eltOneOf(a, 0).add_modified(Intern.intern(new long[] {0, 1}), DOESNT_MATTER));

    // The elements of an int[] variable are not booleans.
    VarInfo i = newIntArrayVarInfo("i[]");
    EltOneOf iOne = eltOneOf(i, 1);
    assertFalse(iOne.is_boolean());
    assertEquals("i[] elements == 1", iOne.format_using(OutputFormat.DAIKON));
  }

  @Test
  public void testNonNonNonNull() {
    @NonNull OneOfSequence inv1 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);
    @NonNull OneOfSequence inv2 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);

    inv1.add_modified(Intern.intern(new long[] {19, 23}), DOESNT_MATTER);
    inv2.add_modified(Intern.intern(new long[] {91, 0}), DOESNT_MATTER);

    assertFalse(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNonNonNonNon() {
    @NonNull OneOfSequence inv1 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);
    @NonNull OneOfSequence inv2 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);

    inv1.add_modified(Intern.intern(new long[] {19, 23}), DOESNT_MATTER);
    inv2.add_modified(Intern.intern(new long[] {91, 32}), DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNonNullNonNull() {
    @NonNull OneOfSequence inv1 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);
    @NonNull OneOfSequence inv2 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);

    inv1.add_modified(Intern.intern(new long[] {19, 0}), DOESNT_MATTER);
    inv2.add_modified(Intern.intern(new long[] {91, 0}), DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  @Test
  public void testNullNullNullNull() {
    @NonNull OneOfSequence inv1 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);
    @NonNull OneOfSequence inv2 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);

    inv1.add_modified(Intern.intern(new long[] {0, 0}), DOESNT_MATTER);
    inv2.add_modified(Intern.intern(new long[] {0, 0}), DOESNT_MATTER);

    assertTrue(inv1.isSameFormula(inv2));
  }

  @Test
  public void testDifferentLengths() {
    @NonNull OneOfSequence inv1 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);
    @NonNull OneOfSequence inv2 = (OneOfSequence) OneOfSequence.get_proto().instantiate(slicex);

    inv1.add_modified(Intern.intern(new long[] {0, 0, 0}), DOESNT_MATTER);
    inv2.add_modified(Intern.intern(new long[] {0, 0}), DOESNT_MATTER);

    assertFalse(inv1.isSameFormula(inv2));
  }
}
