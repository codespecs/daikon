package daikon.test.inv.unary.sequence;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.FileIO;
import daikon.PptSlice;
import daikon.PptSlice1;
import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarComparabilityNone;
import daikon.VarInfo;
import daikon.VarInfoAux;
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
    FileIO.new_decl_format = true;
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
