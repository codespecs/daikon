package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

public class SequenceScalarSubscript extends BinaryDerivation {

  // var_info1 is the sequence
  // var_info2 is the scalar
  public VarInfo seqvar() { return var_info1; }
  public VarInfo sclvar() { return var_info2; }

  private final int index_shift;

  public SequenceScalarSubscript(VarInfo vi1, VarInfo vi2, boolean less1) {
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
    int val2 = var_info2.getIntValue(full_vt) + index_shift;
    if ((val2 < 0) || (val2 >= val1_array.length))
      return ValueAndModified.MISSING;
    int val = val1_array[val2];
    int mod = (((mod1 == ValueTuple.UNMODIFIED)
		&& (mod2 == ValueTuple.UNMODIFIED))
	       ? ValueTuple.UNMODIFIED
	       : ValueTuple.MODIFIED);
    return new ValueAndModified(new Integer(val), mod);
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
    VarComparability compar = var_info1.comparability.elementType();
    return new VarInfo(name, type, rep_type, compar);
  }

  // Eventually incorporate this code.

    // # Determine whether it is constant; if so, ignore.
    // # Perhaps also check that it is within range at least once
    // # (or even every time); if not, not very interesting.
    // if ((not seq_info.is_derived)
    //     and (not scalar_value_1)
    //     and (seq_size_idx != "known_var")
    //     and (scl_inv.max <= var_infos[seq_size_idx].invariant.max)):
    //     lackwit_elt_type = lackwit_type_element_type_alias(seq_info)
    //     proglang_elt_type = seq_info.type.element_type()
    //     var_infos.append(var_info("%s[%s]" % (seq_name, scl_name), proglang_elt_type, lackwit_elt_type, len(var_infos), true))

    //     for new_values in var_new_values.values():
    // 	(this_seq,this_seq_mod) = new_values[seqidx]
    // 	(this_scl,this_scl_mod) = new_values[sclidx]
    // 	if ((this_seq != None) and (this_scl != None)
    // 	    and (this_scl < len(this_seq)) and (this_scl >= 0)):
    // 	    new_values.append(this_seq[this_scl],(this_seq_mod or this_scl_mod))
    // 	else:
    // 	    new_values.append(None,(this_seq_mod or this_scl_mod))

}
