package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public final class SequenceScalarSubsequence extends BinaryDerivation {

  // var_info1 is the sequence
  // var_info2 is the scalar
  public VarInfo seqvar() { return var_info1; }
  public VarInfo sclvar() { return var_info2; }

  // Indicates whether the subscript is an index of valid data or a limit
  // (one element beyond the data of interest).
  public final int index_shift;

  public SequenceScalarSubsequence(VarInfo vi1, VarInfo vi2, boolean less1) {
    super(vi1, vi2);
    if (less1)
      index_shift = -1;
    else
      index_shift = 0;
  }

  public ValueAndModified computeValueAndModified(ValueTuple full_vt) {
    int mod1 = var_info1.getModified(full_vt);
    if (mod1 == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    int mod2 = var_info2.getModified(full_vt);
    if (mod2 == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val1 = var_info1.getValue(full_vt);
    if (val1 == null)
      return ValueAndModified.MISSING;
    long[] val1_array = (long[]) val1;
    int val2 = var_info2.getIndexValue(full_vt);
    // len is the number of elements in the subsequence (that's why we add 1).
    int len = val2+1+index_shift;
    if ((len < 0) || (len > val1_array.length))
      return ValueAndModified.MISSING;
    int mod = (((mod1 == ValueTuple.UNMODIFIED)
		&& (mod2 == ValueTuple.UNMODIFIED))
	       ? ValueTuple.UNMODIFIED
	       : ValueTuple.MODIFIED);
    // One could argue that if the index is greater than the length, one
    // should just return the whole array; but I don't do that.
    // // If the length is longer than the actual array, return the whole array.
    // if (len >= val1_array.length)
    //   return new ValueAndModified(val1, mod);
    if (len == val1_array.length)
      return new ValueAndModified(val1, mod);
    // System.out.println(getVarInfo().name + " for " + ArraysMDE.toString(val1_array) + ";" + val2 + " => " + ArraysMDE.toString(ArraysMDE.subarray(val1_array, 0, len)));
    long[] subarr = ArraysMDE.subarray(val1_array, 0, len);
    subarr = Intern.intern(subarr);
    return new ValueAndModified(subarr, mod);
  }

  protected VarInfo makeVarInfo() {
    String index_shift_string = ((index_shift == 0)
				 ? ""
				 : ((index_shift < 0)
				    ? Integer.toString(index_shift)
				    : "+" + index_shift));
    VarInfo seqvar = seqvar();
    VarInfo sclvar = sclvar();
    String name = seqvar.name
      + "[0.." + sclvar.name + index_shift_string + "]";
    return new VarInfo(name, seqvar.type, seqvar.rep_type, seqvar.comparability);
  }

}
