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
		       null); // null Comparability
  }

  public static PptTopLevel makePptTopLevel(String pptname, VarInfo[] vars)
  {
    PptTopLevel ppt = new PptTopLevel(pptname, vars);
    ppt.invflow_ppts = new PptTopLevel[0];
    ppt.invflow_transforms = new int[0][];
    return ppt;
  }

}
