package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

// like SequenceMax; if one changes, change the other, too
public final class SequenceMin extends UnaryDerivation {

  public SequenceMin(VarInfo vi) {
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
    return new ValueAndModified(Intern.internedInteger(ArraysMDE.min(val_array)),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    String name = "min(" + var_info.name + ")";
    ProglangType ptype = var_info.type.elementType();
    ProglangType rtype = var_info.rep_type.elementType();
    VarComparability comp = var_info.comparability.elementType();
    return new VarInfo(name, ptype, rtype, comp);
  }

}
