package daikon.test.inv.unary.scalar;

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
import daikon.inv.unary.scalar.OneOfScalar;
import daikon.test.Common;
import junit.framework.*;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.junit.BeforeClass;
import org.junit.Test;

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
    FileIO.new_decl_format = true;
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
