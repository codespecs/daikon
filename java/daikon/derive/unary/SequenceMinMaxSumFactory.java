package daikon.derive.unary;

import daikon.*;

public class SequenceMinMaxSumFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    // System.out.println("SequenceMinMaxSumFactory.instantiate(" + vi.name + ")");

    if (vi.rep_type != ProglangType.INT_ARRAY)
      return null;
    if (! vi.type.isArray())
      return null;
    ProglangType elttype = vi.type.elementType();
    if (! elttype.isIntegral())
      return null;
    if (elttype.base() == "char") // interned
      return null;

    UnaryDerivation[] result = new UnaryDerivation[3];
    result[0] = new SequenceMin(vi);
    result[1] = new SequenceMax(vi);
    result[2] = new SequenceSum(vi);
    return result;
  }

}
