package daikon.derive.binary;

import daikon.*;

// This controls derivations which use the scalar as an index into the
// sequence, such as getting the element at that index or a subsequence up
// to that index.

public class SequenceScalarSubscriptFactory extends BinaryDerivationFactory {

  // When calling/creating the derivations, arrange that:
  //   var_info1 is the sequence
  //   var_info2 is the scalar

  public BinaryDerivation[] instantiate(VarInfo vi1, VarInfo vi2) {
    BinaryDerivation[] result = new BinaryDerivation[4];
    result[0] = new SequenceScalarSubscript(vi1, vi2, false);
    result[1] = new SequenceScalarSubscript(vi1, vi2, true);
    result[0] = new SequenceScalarSubsequence(vi1, vi2, false);
    result[1] = new SequenceScalarSubsequence(vi1, vi2, true);
    return result;
  }

  public boolean applicable(VarInfo vi1, VarInfo vi2) {
    // This isn't the very most efficient way to do this, but at least it's
    // comprehensible.
    VarInfo seqvi;
    VarInfo sclvi;

    if (vi1.type.isArray() && (vi2.type.elementType() != ProglangType.INT)) {
      seqvi = vi1;
      sclvi = vi2;
    } else if (vi2.type.isArray() && (vi1.type.elementType() != ProglangType.INT)) {
      seqvi = vi2;
      sclvi = vi1;
    } else {
      return false;
    }

    // I need to incorporate all this code eventually.

    // seq_info = var_infos[seqidx]
    // scl_info = var_infos[sclidx]
    // seq_name = seq_info.name
    // scl_name = scl_info.name
    // scl_inv = scl_info.invariant
    // seq_size_idx = seq_info.derived_len

    // ## This makes absoulely no sense; I've left it commented out only
    // ## so I don't get tempted to do something so silly again.
    // # # Do nothing if the size is known (there's some other var practically
    // # # equal to the size).
    // # if seq_size_idx == "known_var":
    // #     return

    // # For now, do nothing if the scalar is itself derived.
    // if scl_info.is_derived:
    //     return
    // # For now, do nothing if the sequence is itself derived.
    // if seq_info.is_derived:
    //     return

    // # Do nothing if this scalar is actually the size of this sequence
    // if seq_size_idx == sclidx:
    //     return
    // # Another check for scalar being the size of this sequence: sclidx may
    // # not be canonical, but seq_size_idx certainly is, because
    // # we don't call the introduction functions with non-canonical arguments.
    // assert scl_info.is_canonical()
    // if seq_size_idx != "known_var":
    //     if sclidx == var_infos[seq_size_idx].canonical_var():
    // 	return

    // #     if seq_size_idx == "no_var":
    // #         print "sequence %s (size: no_var) and scalar %s (index: %d) unrelated" % (seq_name, scl_name, sclidx)
    // #     else:
    // #         print "sequence %s (size: %s, size index = %s) and scalar %s (index: %d) unrelated" % (seq_name, var_infos[seq_size_idx].name, seq_size_idx, scl_name, sclidx)

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


    // should check varcomparability here
    return true;
  }

}
