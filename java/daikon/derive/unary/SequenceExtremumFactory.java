package daikon.derive.unary;

import daikon.*;
import utilMDE.*;

public class SequenceExtremumFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    // System.out.println("SequenceExtremumFactory.instantiate(" + vi + ")");
    // return (UnaryDerivation)new SequenceFirst(vi);

    if (vi.rep_type != ProglangType.INT_ARRAY)
      return null;

    // System.out.println("SequenceExtremum.applicable(" + vi.name + ") = "
    //                    + SequenceExtremum.applicable(vi));

    if (!SequenceExtremum.applicable(vi)) {
      Global.tautological_suppressed_derived_variables += 4;
      return null;
    }

    // by default, we use the indices 0, 1, -1, -2.
    int lowerbound = -2;
    int upperbound = 1;

    // If the length is the constant 0 or 1, adjust the bounds accordingly.
    VarInfo lengthvar = vi.sequenceSize();
    if (lengthvar.isConstant()) {
      int length_constant = ((Long) lengthvar.constantValue()).intValue();
      if (length_constant == 0) {
        Global.tautological_suppressed_derived_variables += 4;
        return null;
      } else if (length_constant <= 4) {
        lowerbound = 0;
        upperbound = length_constant - 1;
      }
    }

    boolean suppress_zero = false;
    // We know that var.~ll~[0] == var and var.~ll~.field[0] == var.field.
    if (vi.name.indexOf("~ll~") != -1) {
      suppress_zero = true;
      if ((lowerbound == 0) && (upperbound == 0))
        Global.tautological_suppressed_derived_variables += 4;
        return null;
    }

    int num_invs = upperbound - lowerbound + 1 - (suppress_zero ? 1 : 0);
    Assert.assert(num_invs > 0,
                  "No SequenceExtremum invariants to instantiate; "
                  + "lowerbound=" + lowerbound
                  + ", upperbound=" + upperbound
                  + ", suppress_zero=" + suppress_zero);
    UnaryDerivation[] result = new UnaryDerivation[num_invs];
    int j=0;
    for (int i=lowerbound; i<=upperbound; i++) {
      if (! ((i == 0) && suppress_zero)) {
        result[j] = new SequenceExtremum(vi, i);
        j++;
      }
    }
    // No longer needed (I hope!).
    // Assert.assert(j == num_invs,
    //               "SequenceExtremum(" + vi.name + "): "
    //               + "j=" + j + ", num_invs=" + num_invs
    //               + ",lowerbound=" + lowerbound
    //               + ", upperbound=" + upperbound
    //               + ", suppress_zero=" + suppress_zero);

    Global.tautological_suppressed_derived_variables += 4 - num_invs;
    return result;
  }


  // TO DO:

    // # Add each individual element.
    // ## For now, add if not a derived variable; a better test is if
    // ## not a prefix subsequence (sequence slice) we have added.
    // if not seq_var_info.is_derived:
    //     seq_len_inv = var_infos[seq_var_info.derived_len].invariant
    //     assert isinstance(seq_var_info, var_info)
    //     len_min = seq_len_inv.min or 0
    //     # The point of this is not to do checks over every last irrelevant
    //     # element; just look at the one or two at the beginning and the end.
    //     len_min = min(2, len_min)
    //     if len_min > 0:
    // 	for i in range(0, len_min):
    // 	    var_infos.append(var_info("%s[%d]" % (seqvar, i), proglang_elt_type, lackwit_elt_type, len(var_infos), true))
    // 	for new_values in var_new_values.values():
    // 	    for i in range(0, len_min):
    // 		(seq,seq_mod) = new_values[seqidx]
    // 		if seq == None:
    // 		    elt_val = None
    // 		else:
    // 		    elt_val = seq[i]
    // 		new_values.append((elt_val,seq_mod))
    // 	if len_min != seq_len_inv.max:
    // 	    for i in range(-len_min, 0):
    // 		var_infos.append(var_info("%s[%d]" % (seqvar, i), proglang_elt_type, lackwit_elt_type, len(var_infos), true))
    // 	    for new_values in var_new_values.values():
    // 		for i in range(-len_min, 0):
    // 		    (seq,seq_mod) = new_values[seqidx]
    // 		    if seq == None:
    // 			elt_val = None
    // 		    else:
    // 			elt_val = seq[i]
    // 		    new_values.append((elt_val,seq_mod))

}
