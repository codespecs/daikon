package daikon.inv.binary.twoSequence;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.*;

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

    if (Daikon.check_program_types
        && (! var1.type.elementType().comparable(var2.type.elementType()))) {
      // System.out.println("These have different program types: :  "
      //                    + var1.name + " (" + var1.type.format() + ") " + var2.name +  " (" + var2.type.format() + ") ");
      return null;
    }
    // System.out.println("These have comparable program types: :  "
    //                    + var1.name + " (" + var1.type.format() + ") " + var2.name +  " (" + var2.type.format() + ") ");
    if (! Daikon.ignore_comparability) {
      VarComparability compar1 = var1.comparability.elementType();
      VarComparability compar2 = var2.comparability.elementType();
      // The "name" arguments here are wrong.
      if (! VarComparability.compatible(var1.name, compar1, var2.name, compar2)) {
        return null;
      }
    }

    VarInfo super1 = var1.isDerivedSubSequenceOf();
    if (super1 == null)
      super1 = var1;
    VarInfo super2 = var2.isDerivedSubSequenceOf();
    if (super2 == null)
      super2 = var2;

    // System.out.println("TwoSequenceFactory(pass " + pass + ") " + ppt.name
    //                    + "      super1 = " + super1.name + ", super2 = " + super2.name);

    Vector result = new Vector();
    if (pass == 1) {
      // This was test disabled because it resulted in preventing a comparison for
      // this.theArray[this.front..], this.theArray[orig(this.front)+1..]
      // which are actually equal.
      // I decided that the latter shouldn't even be generated -- we should
      // know the relationship between "this.front" and
      // "orig(this.front)+1" -- and re-enabled the test.
      if (super1 == super2) {
        Global.implied_false_noninstantiated_invariants++;
        // System.out.println("No SeqComparison because same super for " + ppt.name);
        LinearBinary lb = LinearBinary.find(ppt);
        if (lb != null)
          System.out.println("  " + lb.format());
      } else {
        result.add(SeqComparison.instantiate(ppt));
      }
    } else if (pass == 2) {
      result.add(Reverse.instantiate(ppt));
      if (super1 == super2) {
        Global.subexact_noninstantiated_invariants += 2;
        Global.implied_false_noninstantiated_invariants += 2 + 2 * Functions.unaryFunctions.length;
      } else {
        Assert.assert(Intern.isInterned(super1.name));
        Assert.assert(Intern.isInterned(super2.name));
        Assert.assert(super1.name != super2.name);

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
