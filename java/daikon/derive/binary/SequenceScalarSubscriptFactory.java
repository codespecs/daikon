package daikon.derive.binary;

import daikon.*;

import utilMDE.*;

// This controls derivations which use the scalar as an index into the
// sequence, such as getting the element at that index or a subsequence up
// to that index.

public class SequenceScalarSubscriptFactory extends BinaryDerivationFactory {

  // When calling/creating the derivations, arrange that:
  //   var_info1 is the sequence
  //   var_info2 is the scalar

  public BinaryDerivation[] instantiate(VarInfo vi1, VarInfo vi2) {
    // This isn't the very most efficient way to do this, but at least it's
    // comprehensible.
    VarInfo seqvar;
    VarInfo sclvar;

    if (vi1.rep_type.equals(ProglangType.INT_ARRAY)
        && (vi2.rep_type.equals(ProglangType.INT))) {
      seqvar = vi1;
      sclvar = vi2;
    } else if (vi2.rep_type.equals(ProglangType.INT_ARRAY)
               && (vi1.rep_type.equals(ProglangType.INT))) {
      seqvar = vi2;
      sclvar = vi1;
    } else {
      return null;
    }

    // I need to incorporate all this code eventually.

    // For now, do nothing if the sequence is itself derived.
    if (seqvar.derived != null)
      return null;
    // For now, do nothing if the scalar is itself derived.
    if (sclvar.derived != null)
      return null;

    VarInfo seqsize = seqvar.sequenceSize();
    // System.out.println("BinaryDerivation.instantiate: sclvar=" + sclvar.name
    //                    + ", sclvar_rep=" + sclvar.canonicalRep().name
    //                    + ", seqsize=" + seqsize.name
    //                    + ", seqsize_rep=" + seqsize.canonicalRep().name);
    if (sclvar.canonicalRep() == seqsize.canonicalRep())
      return null;

    // Find an IntComparison relationship over the two variables, if possible.


    // # Another check for scalar being the size of this sequence: sclidx may
    // # not be canonical, but seq_size_idx certainly is, because
    // # we don't call the introduction functions with non-canonical arguments.
    // assert scl_info.is_canonical()
    // if seq_size_idx != "known_var":
    //     if sclidx == var_infos[seq_size_idx].canonical_var():
    // 	return

    // if not lackwit_types_compatible(scl_info.name, scl_info.lackwit_type,
    // 				"%s-index%d" % (seq_info.name, 1),
    // 				lackwit_type_index_type(seq_info.lackwit_type, 1)):
    //     return

    // # If the scalar is a known constant, record that.
    // if scl_inv.is_exact():
    //     sclconst = scl_inv.min
    // else:
    //     sclconst = None

    // # If the scalar is the constant 0, do nothing (we already extract array[0],
    // # and the subarrays array[0..-1] and array[0..0] are not interesting).
    // # if sclconst == 0:
    // if sclconst != None and sclconst < 1:
    //     return


    // End of applicability tests; now actually create the variables

    BinaryDerivation[] result = new BinaryDerivation[4];
    result[0] = new SequenceScalarSubscript(seqvar, sclvar, false);
    result[1] = new SequenceScalarSubscript(seqvar, sclvar, true);
    result[2] = new SequenceScalarSubsequence(seqvar, sclvar, false);
    result[3] = new SequenceScalarSubsequence(seqvar, sclvar, true);
    return result;
  }

}
