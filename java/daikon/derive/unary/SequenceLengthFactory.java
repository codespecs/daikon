package daikon.derive.unary;

import daikon.*;

public final class SequenceLengthFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    // if (vi.rep_type != ProglangType.INT_ARRAY)
    //   return null;
    if (! vi.rep_type.isArray())
      return null;
    if ((vi.name instanceof VarInfoName.TypeOf) || 
        (vi.isOrigVar() && (vi.postState.name instanceof VarInfoName.TypeOf))) {
      return null;
    }

    if (! SequenceLength.applicable(vi)) {
      Global.tautological_suppressed_derived_variables++;
      return null;
    }

    return new UnaryDerivation[] { new SequenceLength(vi, 0),
                                   new SequenceLength(vi, -1) };
  }

}
