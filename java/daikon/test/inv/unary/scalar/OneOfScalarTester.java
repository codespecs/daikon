package daikon.test.inv.unary.scalar;

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

@SuppressWarnings("nullness") // testing code
public class OneOfScalarTester extends TestCase {

  private VarInfo[] vars = {Common.makeHashcodeVarInfo("x"), Common.newIntVarInfo("y")};
  private PptTopLevel ppt = Common.makePptTopLevel("Foo.Baa(int):::ENTER", vars);
  private PptSlice slicex = new PptSlice1(ppt, new VarInfo[] {vars[0]});
  // private PptSlice slicey = new PptSlice1(ppt, new VarInfo[] {vars[1]});

  private static final int DOESNT_MATTER = 0;

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs(daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(OneOfScalarTester.class));
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

  public OneOfScalarTester(String name) {
    super(name);
  }

  public void testNullNon() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assert !inv1.isSameFormula(inv2);
  }

  public void testNullNull() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assert inv1.isSameFormula(inv2);
  }

  public void testNonNon() {
    @NonNull OneOfScalar inv1 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);
    @NonNull OneOfScalar inv2 = (OneOfScalar) OneOfScalar.get_proto().instantiate(slicex);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assert inv1.isSameFormula(inv2);
  }

  /* NEED TO DEFINE SEMANTICS WITH MIKE E
  public void testNullNonHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assert ! inv1.isSameFormula(inv2);
  }

  public void testNullNullHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(0, DOESNT_MATTER);
    inv2.add_modified(0, DOESNT_MATTER);

    assert inv1.isSameFormula(inv2);
  }

  public void testNonNonHashcodeInt() {
    OneOfScalar inv1 = OneOfScalar.get_proto().instantiate(slicex);
    OneOfScalar inv2 = OneOfScalar.get_proto().instantiate(slicey);

    inv1.add_modified(19, DOESNT_MATTER);
    inv2.add_modified(22, DOESNT_MATTER);

    assert inv1.isSameFormula(inv2);
  }
  */
}
