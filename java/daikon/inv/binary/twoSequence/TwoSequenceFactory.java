package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;

import utilMDE.*;

import java.util.*;

public final class TwoSequenceFactory {

  // Add the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static Vector instantiate(PptSlice ppt, int pass) {
    Assert.assert(ppt.arity == 2);
    // Not really the right place for these tests
    VarInfo var1 = ppt.var_infos[0];
    VarInfo var2 = ppt.var_infos[1];

    Assert.assert((var1.rep_type == ProglangType.INT_ARRAY)
                  && (var2.rep_type == ProglangType.INT_ARRAY));

    VarInfo super1 = var1.isDerivedSubSequenceOf();
    if (super1 == null)
      super1 = var1;
    VarInfo super2 = var2.isDerivedSubSequenceOf();
    if (super2 == null)
      super2 = var2;

    if (Daikon.check_program_types
        && (! var1.type.comparable(var2.type)))
      return null;

    Vector result = new Vector();
    if (pass == 1) {
      if (super1 == super2) {
        Global.implied_false_noninstantiated_invariants++;
      } else {
        result.add(SeqComparison.instantiate(ppt));
      }
    } else if (pass == 2) {
      result.add(Reverse.instantiate(ppt));
      if (super1 == super2) {
        Global.subexact_noninstantiated_invariants += 2;
        Global.implied_false_noninstantiated_invariants += 2 + 2 * Functions.unaryFunctions.length;
      } else {
        // NonEqual.instantiate(ppt);
        result.add(SubSequence.instantiate(ppt));

        result.add(PairwiseIntComparison.instantiate(ppt));
        result.add(PairwiseLinearBinary.instantiate(ppt));
        for (int i=0; i<2; i++) {
          boolean invert = (i==1);
          VarInfo arg = (invert ? var1 : var2);
          // Don't bother to check arg.isConstant():  we really want to
          // know whether the elements of arg are constant.
          for (int j=0; j<Functions.unaryFunctions.length; j++) {
            result.add(PairwiseFunctionUnary.instantiate(ppt, Functions.unaryFunctionNames[j], Functions.unaryFunctions[j], invert));
          }
        }
      }
    }
    return result;
  }

  private TwoSequenceFactory() {
  }

}
