package daikon.derive.unary;

import daikon.*;

public final class SequenceMinMaxSumFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    // System.out.println("SequenceMinMaxSumFactory.instantiate(" + vi.name + ")");

    if (vi.rep_type != ProglangType.INT_ARRAY)
      return null;
    if (! vi.type.isArray())
      return null;
    if (! vi.type.elementIsIntegral())
      return null;
    if (vi.type.base() == "char") // interned
      return null;
    // Should be reversed at some point; for now, will improve runtime.
    if (Daikon.output_style != Daikon.OUTPUT_STYLE_NORMAL)
      return null;

    return new UnaryDerivation[] {
      new SequenceMin(vi),
      new SequenceMax(vi),
      new SequenceSum(vi),
    };
  }

}
