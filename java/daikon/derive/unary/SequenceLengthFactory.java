package daikon.derive.unary;

import daikon.*;

public class SequenceLengthFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    return new UnaryDerivation[] { new SequenceLength(vi) };
  }

  public boolean applicable(VarInfo vi) {
    return SequenceLength.applicable(vi);
  }

}
