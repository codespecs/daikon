package daikon.derive.unary;

import daikon.*;

public final class SequenceMinMaxSumFactory extends UnaryDerivationFactory {

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
    // Should be reversed at some point; for now, will improve runtime.m
    if (Daikon.esc_output)
      return null;

    return new UnaryDerivation[] {
      new SequenceMin(vi),
      new SequenceMax(vi),
      new SequenceSum(vi),
    };
  }

}
