package daikon.inv.unary.stringsequence;

import daikon.*;

import utilMDE.*;

import java.util.*;

public final class SingleStringSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt) {

    VarInfo var = ppt.var_infos[0];
    Assert.assertTrue(var.rep_type == ProglangType.STRING_ARRAY);


    Vector result = new Vector();
    result.add(OneOfStringSequence.instantiate(ppt));
      // // I'm not checking var.isConstant() for now
      // result.add(EltwiseIntComparison.instantiate(ppt));
      result.add(EltOneOfString.instantiate(ppt));
      // result.add(CommonStringSequence.instantiate(ppt));
      // // result.add(EltNonZero.instantiate(ppt));
      // // result.add(NoDuplicates.instantiate(ppt));
    return result;
  }

  private SingleStringSequenceFactory() {
  }

}
