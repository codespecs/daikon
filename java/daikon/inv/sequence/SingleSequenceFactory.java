package daikon.inv.sequence;

import daikon.*;

import utilMDE.*;

import java.util.*;

public class SingleSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    // return (UnaryDerivation)new SequenceFirst(vi);

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.rep_type == ProglangType.INT_ARRAY);


    Vector result = new Vector();
    if (pass == 1) {
      result.add(OneOfSequence.instantiate(ppt));
    } else if (pass == 2) {
      // I'm not checking var.isConstant() for now
      result.add(EltIntComparison.instantiate(ppt));
    }
    return result;
  }

  private SingleSequenceFactory() {
  }

}
