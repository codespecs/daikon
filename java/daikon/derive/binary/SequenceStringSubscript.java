package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public final class SequenceStringSubscript extends BinaryDerivation {

  // base1 is the sequence
  // base2 is the scalar
  public VarInfo seqvar() { return base1; }
  public VarInfo sclvar() { return base2; }

  // Indicates whether the subscript is an index of valid data or a limit
  // (one element beyond the data of interest).
  public final int index_shift;

  public SequenceStringSubscript(VarInfo vi1, VarInfo vi2, boolean less1) {
    super(vi1, vi2);
    if (less1)
      index_shift = -1;
    else
      index_shift = 0;
  }

  public ValueAndModified computeValueAndModified(ValueTuple full_vt) {
    int mod1 = base1.getModified(full_vt);
    if (mod1 == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    int mod2 = base2.getModified(full_vt);
    if (mod2 == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val1 = base1.getValue(full_vt);
    if (val1 == null)
      return ValueAndModified.MISSING;
    String[] val1_array = (String[]) val1;
    int val2 = base2.getIndexValue(full_vt) + index_shift;
    if ((val2 < 0) || (val2 >= val1_array.length))
      return ValueAndModified.MISSING;
    String val = val1_array[val2];
    int mod = (((mod1 == ValueTuple.UNMODIFIED)
		&& (mod2 == ValueTuple.UNMODIFIED))
	       ? ValueTuple.UNMODIFIED
	       : ValueTuple.MODIFIED);
    return new ValueAndModified(val, mod);
  }

  protected VarInfo makeVarInfo() {
    String index_shift_string = ((index_shift == 0)
				 ? ""
				 : ((index_shift < 0)
				    ? Integer.toString(index_shift)
				    : "+" + index_shift));
    VarInfo seqvar = seqvar();
    String name = seqvar.name
      + "[" + sclvar().name + index_shift_string + "]";
    ProglangType type = seqvar.type.elementType();
    ProglangType rep_type = seqvar.rep_type.elementType();
    VarComparability compar = base1.comparability.elementType();
    return new VarInfo(name, type, rep_type, compar);
  }

}
