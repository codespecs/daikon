package daikon.inv.unary.sequence;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleFloatSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.rep_type == ProglangType.DOUBLE_ARRAY);


    Vector result = new Vector();
    // I guess there are none?
    return result;
  }

  private SingleFloatSequenceFactory() {
  }

}
