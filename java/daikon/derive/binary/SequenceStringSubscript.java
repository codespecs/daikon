package daikon.derive.binary;

import daikon.*;
import daikon.derive.*;

import utilMDE.*;

// *****
// Automatically generated from SequenceSubscript-cpp.java
// *****

public final class SequenceStringSubscript  extends BinaryDerivation {

  // base1 is the sequence
  // base2 is the scalar
  public VarInfo seqvar() { return base1; }
  public VarInfo sclvar() { return base2; }

  // Indicates whether the subscript is an index of valid data or a limit
  // (one element beyond the data of interest).
  // Value is -1 or 0.
  public final int index_shift;

  public SequenceStringSubscript (VarInfo vi1, VarInfo vi2, boolean less1) {
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
    String [] val1_array = (String []) val1;
    int val2 = base2.getIndexValue(full_vt) + index_shift;
    if ((val2 < 0) || (val2 >= val1_array.length))
      return ValueAndModified.MISSING;
    String  val = val1_array[val2];
    int mod = (((mod1 == ValueTuple.UNMODIFIED)
		&& (mod2 == ValueTuple.UNMODIFIED))
	       ? ValueTuple.UNMODIFIED
	       : ValueTuple.MODIFIED);
    return new ValueAndModified( val  , mod);
  }

  protected VarInfo makeVarInfo() {
    String index_shift_string = ((index_shift == 0)
				 ? ""
				 : ((index_shift < 0)
				    ? Integer.toString(index_shift)
				    : "+" + index_shift));
    VarInfo seqvar = seqvar();
    VarInfo sclvar = sclvar();
    // next two only for nameing; types may be wrong
    String seqvar_name = seqvar.name;
    String seqvar_esc_name = seqvar.esc_name;
    String sclvar_name = sclvar.name;
    String sclvar_esc_name = sclvar.esc_name;
    boolean add_orig = false;
    if (seqvar.isOrigVar()) {
      String[] seqvar_names = seqvar.postStateEquivalent();
      if (seqvar_names == null) {
        seqvar_name = seqvar.postState.name;
        seqvar_esc_name = seqvar.postState.esc_name;
        // Need to wrap this expressoin in orig() afterward.
        add_orig = true;

        // No poststate equivalent; make sure that index is also in prestate.
        if (sclvar.isOrigVar()) {
          Assert.assert(sclvar.isOrigVar());
          sclvar_name = sclvar.postState.name;
          sclvar_esc_name = sclvar.postState.esc_name;
        } else {
          // If this variable wasn't modified by this routine, the same
          // expression works in the prestate as in the poststate.
          VarInfo orig = sclvar.ppt.findVar(VarInfo.makeOrigName(sclvar.name));
          if ((orig != null) && (orig.equal_to != sclvar.equal_to)) {
            // otherwise, find a var in the prestate that equals this
            String sclvar_names[] = sclvar.preStateEquivalent();
            if (sclvar_names == null) {
              sclvar_name = "post(" + sclvar_name + ")";
              sclvar_esc_name = "\new(" + sclvar_esc_name + ")";
              // throw new Error("can't find prestate index (or poststate array) for\n  "
              //                 + seqvar_name + "[" + sclvar_name + "]"
              //                 + "  where " + orig.name + " != " + sclvar.name);
            } else {
              sclvar_name = VarInfo.unOrigName(sclvar_names[0]);
              sclvar_esc_name = VarInfo.unOrigName_esc(sclvar_names[1]);
            }
          }
        }
      } else {
        seqvar_name = seqvar_names[0];
        seqvar_esc_name = seqvar_names[1];
        String[] sclvar_names = sclvar.postStateEquivalent();
        sclvar_name = sclvar_names[0];
        sclvar_esc_name = sclvar_names[1];
      }
    }
    String name = addSubscript(seqvar_name, sclvar_name + index_shift_string);
    String esc_name = addSubscript_esc(seqvar_esc_name, sclvar_esc_name + index_shift_string);
    if (add_orig) {
      name = VarInfo.makeOrigName(name);
      esc_name = VarInfo.makeOrigName_esc(esc_name);
    }
    ProglangType type = seqvar.type.elementType();
    ProglangType rep_type = seqvar.rep_type.elementType();
    VarComparability compar = base1.comparability.elementType();
    return new VarInfo(name, esc_name, type, rep_type, compar);
  }

}
