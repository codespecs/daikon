package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import utilMDE.*;

// This represents a sequence element at a particular offset (such as
// first, second, penultimate, last).

// originally from pass1.
public final class SequenceInitial extends UnaryDerivation {

  public final int index;       // negative if counting from end
  // array length required for the subscript to be meaningful:  (ie, 1 or 2)
  final int minLength;

  public SequenceInitial(VarInfo vi, int index) {
    super(vi);
    this.index = index;
    if (index < 0)
      minLength = -index;
    else
      minLength = index+1;
  }

  public VarInfo seqvar() {
    return var_info;
  }

  public static boolean applicable(VarInfo vi) {
    Assert.assert(vi.rep_type == ProglangType.INT_ARRAY);
    // For now, applicable if not a derived variable; a better test is if
    // not a prefix subsequence (sequence slice) we have added.
    if (vi.derived != null) {
      Assert.assert(vi.derived instanceof SequenceScalarSubsequence);
      return false;
    }

    if (vi.isConstant() && vi.constantValue() == null) {
      return false;
    }

    VarInfo lengthvar = vi.sequenceSize();
    if (lengthvar.isConstant()) {
      long length_constant = ((Long) lengthvar.constantValue()).longValue();
      if (length_constant == 0) {
        return false;
      }
    }

    return true;
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = var_info.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = var_info.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    if (var_info.rep_type == ProglangType.INT_ARRAY) {
      long[] val_array = (long[])val;
      if (val_array.length < minLength)
        return ValueAndModified.MISSING;
      int real_index = (index<0 ? val_array.length + index : index);
      return new ValueAndModified(Intern.internedLong(val_array[real_index]), source_mod);
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
    ProglangType ptype = var_info.type.elementType();
    ProglangType rtype = var_info.rep_type.elementType();
    VarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
