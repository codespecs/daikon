package daikon.derive.unary;

import daikon.*;

public class SequenceMinMaxSumFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    UnaryDerivation[] result = new UnaryDerivation[3];
    result[0] = new SequenceMin(vi);
    result[1] = new SequenceMax(vi);
    result[2] = new SequenceSum(vi);
    return result;
  }

  public boolean applicable(VarInfo vi) {
    // Don't just check rep_type because, for pointers, it is also int.
    return (vi.type == ProglangType.INT_ARRAY);
  }

}
