package daikon.inv.sequence;

import daikon.*;

import utilMDE.*;

public class SingleSequenceFactory {

  // Adds the appropriate new Invariant objects to the specified Invariants
  // collection.
  public static void instantiate(PptSlice ppt, int pass) {
    // return (UnaryDerivation)new SequenceFirst(vi);

    VarInfo var = ppt.var_infos[0];
    Assert.assert(var.type.isArray());


    if (pass == 1) {
      OneOfSequence.instantiate(ppt);
    } else if (pass == 2) {
      // I'm not checking var.isConstant() for now
      EltIntComparison.instantiate(ppt);
    }
  }

  private SingleSequenceFactory() {
  }

}
