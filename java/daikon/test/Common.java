package daikon.test;

import daikon.PptTopLevel;
import daikon.ProglangType;
import daikon.VarComparabilityNone;
import daikon.VarInfo;
import daikon.VarInfoAux;
import java.util.ArrayList;
import java.util.List;

/** A collection of useful helper methods that are common to many different individual tests. */
public class Common {
  private Common() {
    throw new Error("do not instantiate");
  }

  @SuppressWarnings("interning")
  public static VarInfo newIntVarInfo(String name) {
    return new VarInfo(
        name, ProglangType.INT, ProglangType.INT, VarComparabilityNone.it, VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeHashcodeVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.HASHCODE,
        ProglangType.HASHCODE,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeIntArrayVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.INT_ARRAY,
        ProglangType.INT_ARRAY,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  @SuppressWarnings("interning")
  public static VarInfo makeHashcodeArrayVarInfo(String name) {
    return new VarInfo(
        name,
        ProglangType.HASHCODE_ARRAY,
        ProglangType.HASHCODE_ARRAY,
        VarComparabilityNone.it,
        VarInfoAux.getDefault());
  }

  public static PptTopLevel makePptTopLevel(String pptname, VarInfo[] vars) {

    // If any of the variables have enclosing variables, include those in
    // the ppt as well.
    List<VarInfo> vlist = new ArrayList<>();
    for (VarInfo vi : vars) {
      if (vi.enclosing_var != null) {
        vlist.add(vi.enclosing_var);
      }
    }
    if (vlist.size() > 0) {
      VarInfo[] full = new VarInfo[vars.length + vlist.size()];
      int index = 0;
      for (VarInfo vi : vars) {
        full[index++] = vi;
      }
      for (VarInfo vi : vlist) {
        full[index++] = vi;
      }
      vars = full;
    }

    PptTopLevel ppt = new PptTopLevel(pptname, vars);
    return ppt;
  }
}
