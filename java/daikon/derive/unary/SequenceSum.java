package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

public final class SequenceSum extends UnaryDerivation {

  public SequenceSum(VarInfo vi) {
    super(vi);
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = base.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    long[] val_array = (long[])val;
    long result = 0;
    for (int i=0; i<val_array.length; i++)
      result += val_array[i];
    return new ValueAndModified(Intern.internedLong(result),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    VarInfoName name = base.name.applyFunction("sum");
    ProglangType ptype = base.type.elementType();
    ProglangType frtype = base.file_rep_type.elementType();
    VarComparability comp = base.comparability.elementType();
    return new VarInfo(name, ptype, frtype, comp);
  }

}
