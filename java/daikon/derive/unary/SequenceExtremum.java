package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;

// I need to turn this into SequenceExtrema or some such.

// originally from pass1.
public class SequenceExtremum extends UnaryDerivation {

  int index;			// negative if counting from end
  int minLength;

  public SequenceExtremum(VarInfo vi, int index_) {
    super(vi);
    index = index_;
    if (index < 0)
      minLength = -index;
    else
      minLength = index+1;
  }

  public VarInfo seqvar() {
    return var_info;
  }

  public static boolean applicable(VarInfo vi) {
    if (!vi.rep_type.isArray())
      return false;
    // if (vi.is_always_missing_or_null_or_length_0())
    //   return false;
    return true;
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = var_info.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = var_info.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    if (var_info.rep_type.equals(ProglangType.INT_ARRAY)) {
      int[] val_array = (int[])val;
      if (val_array.length < minLength)
        return ValueAndModified.MISSING;
      int real_index = (index<0 ? val_array.length + index : index);
      return new ValueAndModified(new Integer(val_array[real_index]), source_mod);
    } else {
      Object[] val_array = (Object[])val;
      if (val_array.length < minLength)
        return ValueAndModified.MISSING;
      int real_index = (index<0 ? val_array.length + index : index);
      return new ValueAndModified(val_array[real_index], source_mod);
    }
  }

  VarInfo this_var_info;

  public VarInfo makeVarInfo() {
    if (this_var_info != null)
      return this_var_info;

    String name = var_info.name + "[" + index + "]";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    ExplicitVarComparability comp = var_info.comparability.elementType();
    this_var_info = new VarInfo(name, ptype, rtype, comp);

    // Is this appropriate?  Perhaps it should be done elsewhere, more
    // centrally.
    var_info.derivees.add(this);
    this_var_info.derived = this;
    return this_var_info;
  }

}
