package daikon.derive.unary;

import daikon.*;

public class SequenceExtremumFactory extends UnaryDerivationFactory {

  public UnaryDerivation[] instantiate(VarInfo vi) {
    // System.out.println("SequenceExtremumFactory.instantiate(" + vi + ")");
    // return (UnaryDerivation)new SequenceFirst(vi);

    if (!SequenceExtremum.applicable(vi))
      return null;

    UnaryDerivation[] result = new UnaryDerivation[4];
    for (int i=0; i<4; i++)
      // the index is 0, 1, -1, or -2
      result[i] = new SequenceExtremum(vi, i-2);
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
