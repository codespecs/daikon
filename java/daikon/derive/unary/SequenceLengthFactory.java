package daikon.derive.unary;

import daikon.*;

public class SequenceLengthFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    if (! SequenceLength.applicable(vi))
      return null;

    return new UnaryDerivation[] { new SequenceLength(vi) };
  }

}
