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

  // True if we are deriving from the start of the sequence to the scalar.
  // False if we are deriving from the scalar to the end of the sequence.
  public final boolean from_start;
  
  /**
   * @param from_start true means the range goes 0..n; false means the
   * range goes n..end.  (n might be fudged through off_by_one)
   * @param off_by_one true means we should exclude the scalar from
   * the range; false means we should include it
   **/
  public SequenceScalarSubsequence(VarInfo vi1, VarInfo vi2, boolean from_start, boolean off_by_one) {
    super(vi1, vi2);
    this.from_start = from_start;
    if (off_by_one)
      index_shift = from_start ? -1 : +1;
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

    // One could argue that if the range exceeds the array bounds, one
    // should just return the whole array; but we don't do that.  We
    // say MISSING instead.

    int begin_inclusive, end_exclusive;
    if (from_start) {
      begin_inclusive = 0;
      end_exclusive = val2+index_shift+1; // +1: endpoint is exclusive
      if ((end_exclusive < 0) || (end_exclusive > val1_array.length))
	return ValueAndModified.MISSING;
    } else {
      begin_inclusive = val2+index_shift;
      end_exclusive = val1_array.length;
      if ((begin_inclusive < 0) || (begin_inclusive > val1_array.length))
	return ValueAndModified.MISSING;
    }

    int mod = (((mod1 == ValueTuple.UNMODIFIED)
		&& (mod2 == ValueTuple.UNMODIFIED))
	       ? ValueTuple.UNMODIFIED
	       : ValueTuple.MODIFIED);

    if ((begin_inclusive == 0) && (end_exclusive == val1_array.length))
      return new ValueAndModified(val1, mod);

    long[] subarr = ArraysMDE.subarray(val1_array, begin_inclusive, end_exclusive - begin_inclusive);
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
    String name = from_start ? 
      (seqvar.name + "[0.." + sclvar.name + index_shift_string + "]") :
      (seqvar.name + "[" + sclvar.name + index_shift_string + "..]");
    return new VarInfo(name, seqvar.type, seqvar.rep_type, seqvar.comparability);
  }

}
