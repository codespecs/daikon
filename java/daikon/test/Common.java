package daikon.test;

import daikon.*;

/**
 * A collection of useful helper methods that are common to many
 * differnt individual tests.
 **/
public class Common
{

  public static VarInfo makeIntVarInfo(String name)
  {
    return new VarInfo(VarInfoName.parse(name),
                       ProglangType.INT,
                       ProglangType.INT,
                       null, // null Comparability
                       VarInfoAux.getDefault());
  }

  public static VarInfo makeHashcodeVarInfo(String name)
  {
    return new VarInfo(VarInfoName.parse(name),
                       ProglangType.HASHCODE,
                       ProglangType.HASHCODE,
                       null, // null Comparability
                       VarInfoAux.getDefault());
  }


  public static VarInfo makeIntArrayVarInfo(String name)
  {
    return new VarInfo(VarInfoName.parse(name),
                       ProglangType.INT_ARRAY,
                       ProglangType.INT_ARRAY,
                       null, // null Comparability
                       VarInfoAux.getDefault());
  }

  public static VarInfo makeHashcodeArrayVarInfo(String name)
  {
    return new VarInfo(VarInfoName.parse(name),
                       ProglangType.HASHCODE_ARRAY,
                       ProglangType.HASHCODE_ARRAY,
                       null, // null Comparability
                       VarInfoAux.getDefault());
  }

  public static PptTopLevel makePptTopLevel(String pptname, VarInfo[] vars)
  {
    PptTopLevel ppt = new PptTopLevel(pptname, vars);
    ppt.invflow_ppts = new PptTopLevel[0];
    ppt.invflow_transforms = new int[0][];
    return ppt;
  }

}
