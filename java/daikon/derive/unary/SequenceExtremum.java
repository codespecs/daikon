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

  public SequenceExtremum(VarInfo vi, int index) {
    super(vi);
    this.index = index;
    if (index < 0)
      minLength = -index;
    else
      minLength = index+1;
  }

  public VarInfo seqvar() {
    return base;
  }

  public static boolean applicable(VarInfo vi) {
    Assert.assert(vi.rep_type == ProglangType.INT_ARRAY);
    if (vi.derived != null) {
      Assert.assert(vi.derived instanceof SequenceScalarSubsequence);
      return false;
    }

    // if (vi.is_always_missing_or_null_or_length_0())
    //   return false;
    return true;
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = base.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    if (base.rep_type == ProglangType.INT_ARRAY) {
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
    String name = BinaryDerivation.addSubscript(base.name, "" + index);
    ProglangType ptype = base.type.elementType();
    ProglangType rtype = base.rep_type.elementType();
    VarComparability comp = base.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
