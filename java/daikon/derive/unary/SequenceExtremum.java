package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import utilMDE.*;

// This represents a sequence element at a particular offset (such as
// first, second, penultimate, last).  I should probably find a better
// name.

// I need to turn this into SequenceExtrema or some such.

// originally from pass1.
public class SequenceExtremum extends UnaryDerivation {

  public final int index;       // negative if counting from end
  // array length required for the subscript to be meaningful:  (ie, 1 or 2)
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
    if (! vi.rep_type.equals(ProglangType.INT_ARRAY))
      return false;
    if (vi.derived != null) {
      Assert.assert(vi.derived instanceof SequenceScalarSubsequence);
      return false;
    }

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
      return new ValueAndModified(Intern.internedInteger(val_array[real_index]), source_mod);
    } else {
      Object[] val_array = (Object[])val;
      if (val_array.length < minLength)
        return ValueAndModified.MISSING;
      int real_index = (index<0 ? val_array.length + index : index);
      return new ValueAndModified(val_array[real_index], source_mod);
    }
  }

  protected VarInfo makeVarInfo() {
    String name = var_info.name + "[" + index + "]";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    VarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
