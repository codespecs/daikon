package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;

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
    return new ValueAndModified(new Integer(result), source_mod);
  }

  VarInfo this_var_info;

  public VarInfo makeVarInfo() {
    if (this_var_info != null)
      return this_var_info;

    String name = "sum(" + var_info.name + ")";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    ExplicitVarComparability comp = var_info.comparability.elementType();
    this_var_info = new VarInfo(name, ptype, rtype, comp);

    var_info.derivees.add(this);
    this_var_info.derived = this;
    return this_var_info;
  }

}
