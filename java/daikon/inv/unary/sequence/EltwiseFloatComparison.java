package daikon.inv.unary.sequence;

import daikon.*;

/**
 * Class defined so that the different types of EltwiseIntComparison (and
 * seperately EltwiseFloatComparison), at the current moment those are ==,
 * !=, <, <=, >, >= can have a common superclass which is needed for
 * actions like the isExclusiveFormula method. It is also used to force the
 * definition of the hasSeenNonTrivialSample function for each type of
 * EltwiseIntComparison.
 */

public abstract class EltwiseFloatComparison
  extends SingleFloatSequence
{
  static final long serialVersionUID = 20030109L;

  public static boolean dkconfig_enabled = true;

  abstract public boolean hasSeenNonTrivialSample();

  protected EltwiseFloatComparison(PptSlice ppt) {
    super(ppt);
  }
}
