package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public class SequenceScalarSubsequence extends BinaryDerivation {

  // var_info1 is the sequence
  // var_info2 is the scalar
  public VarInfo seqvar() { return var_info1; }
  public VarInfo sclvar() { return var_info2; }

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
    int[] val1_array = (int[]) val1;
    int val2 = var_info2.getIntValue(full_vt);
    // The length is one less than the value because the indices are
    // inclusive, not exclusive.
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
    int[] subarr = ArraysMDE.subarray(val1_array, 0, len);
    subarr = Intern.intern(subarr);
    return new ValueAndModified(subarr, mod);
  }

  protected VarInfo makeVarInfo() {
    String index_shift_string = ((index_shift == 0)
				 ? ""
				 : ((index_shift < 0)
				    ? Integer.toString(index_shift)
				    : "+" + index_shift));
    String name = var_info1.name
      + "[0.." + var_info2.name + index_shift_string + "]";
    return new VarInfo(name, var_info1.type, var_info1.rep_type, var_info1.comparability);
  }

}


// def introduce_from_sequence_scalar_pass2(var_infos, var_new_values, seqidx, sclidx):
//     assert len(var_infos) == len(var_new_values.values()[0])
//
//     scalar_value_1 = not(sclconst == None or sclconst > 1)
//
//     # Add subsequences
//     if not seq_info.invariant.can_be_None and not seq_info.is_derived and not scl_info.invariant.can_be_None:
//         lackwit_seq_type = lackwit_make_alias(seq_name, seq_info.lackwit_type)
//         full_var_info = var_info("%s[0..%s]" % (seq_name, scl_name), seq_info.type, lackwit_seq_type, len(var_infos), true)
//         # "known_var" means there is a known value, but no variable
//         # holds that particular value.
//         full_var_info.derived_len = "known_var" # length is 1 more than var[sclidx]
//         var_infos.append(full_var_info)
//         assert (not scalar_value_1) or sclconst == 1
//         if not scalar_value_1:
//             less_one_var_info = var_info("%s[0..%s-1]" % (seq_name, scl_name), seq_info.type, lackwit_seq_type, len(var_infos), true)
//             less_one_var_info.derived_len = sclidx
//             var_infos.append(less_one_var_info)
//         for new_values in var_new_values.values():
//             (seq,seq_mod) = new_values[seqidx]
//             (scl,scl_mod) = new_values[sclidx]
//             assert sclconst == None or scl == sclconst
//             if (scl+1 <= len(seq)) and (scl+1 >= 0):
//                 new_value_full = seq[0:scl+1]
//             else:
//                 new_value_full = None
//             new_values.append(new_value_full,(seq_mod or scl_mod))
//             if not scalar_value_1:
//                 if (scl <= len(seq)) and (scl >= 0):
//                     new_value_less_one = seq[0:scl]
//                 else:
//                     new_value_less_one = None
//                 new_values.append(new_value_less_one,(seq_mod or scl_mod))
//             if debug_derive:
//                 print "seq %s = %s (len = %d), scl %s = %s, new_value_less_one = %s" % (seq_name, seq, len(seq), scl_name, scl, new_value_less_one)
