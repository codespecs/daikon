package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;

import utilMDE.*;

public class SequenceMax extends UnaryDerivation {

  public SequenceMax(VarInfo vi) {
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
    if (val_array.length == 0)
      return ValueAndModified.MISSING;
    return new ValueAndModified(new Integer(ArraysMDE.max(val_array)),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    String name = "max(" + var_info.name + ")";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    ExplicitVarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
