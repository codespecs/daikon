package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

public final class SequenceSum extends UnaryDerivation {

  public SequenceSum(VarInfo vi) {
    super(vi);
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = var_info.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = var_info.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    int[] val_array = (int[])val;
    int result = 0;
    for (int i=0; i<val_array.length; i++)
      result += val_array[i];
    return new ValueAndModified(Intern.internedInteger(result),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    String name = "sum(" + var_info.name + ")";
    ProglangType ptype = var_info.type.elementType();
    ProglangType rtype = var_info.rep_type.elementType();
    VarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
