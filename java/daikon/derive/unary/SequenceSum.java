package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

public class SequenceSum extends UnaryDerivation {

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
    int[] ints = (int[])val;
    int result = 0;
    for (int i=0; i<ints.length; i++)
      result += ints[i];
    return new ValueAndModified(Intern.internedInteger(result),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    String name = "sum(" + var_info.name + ")";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    VarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
