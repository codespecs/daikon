package daikon.inv.unary.sequence;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleFloatSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.rep_type == ProglangType.DOUBLE_ARRAY);


    Vector result = new Vector();
    if (pass == 1) {
    } else if (pass == 2) {
    }
    return result;
  }

  private SingleFloatSequenceFactory() {
  }

}
