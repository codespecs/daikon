package daikon.test;

import daikon.*;

/**
 * A collection of useful helper methods that are common to many
 * different individual tests.
 **/
public class Common
{
  private Common() { throw new Error("do not instantiate"); }

  public static VarInfo makeIntVarInfo(String name) {
    return new VarInfo(name,
                       ProglangType.INT,
                       ProglangType.INT,
                       VarComparabilityNone.it,
                       VarInfoAux.getDefault());
  }

  public static VarInfo makeHashcodeVarInfo(String name) {
    return new VarInfo(name,
                       ProglangType.HASHCODE,
                       ProglangType.HASHCODE,
                       VarComparabilityNone.it,
                       VarInfoAux.getDefault());
  }


  public static VarInfo makeIntArrayVarInfo(String name) {
    return new VarInfo(name,
                       ProglangType.INT_ARRAY,
                       ProglangType.INT_ARRAY,
                       VarComparabilityNone.it,
                       VarInfoAux.getDefault());
  }

  public static VarInfo makeHashcodeArrayVarInfo(String name) {
    return new VarInfo(name,
                       ProglangType.HASHCODE_ARRAY,
                       ProglangType.HASHCODE_ARRAY,
                       VarComparabilityNone.it,
                       VarInfoAux.getDefault());
  }

  public static PptTopLevel makePptTopLevel(String pptname, VarInfo[] vars) {
    PptTopLevel ppt = new PptTopLevel(pptname, vars);
    return ppt;
  }

}
