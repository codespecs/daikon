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
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = base.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING;
    long[] val_array = (long[])val;
    if (val_array.length == 0)
      return ValueAndModified.MISSING;
    return new ValueAndModified(Intern.internedLong(ArraysMDE.min(val_array)),
                                source_mod);
  }

  protected VarInfo makeVarInfo() {
    String name = "min(" + base.name + ")";
    String esc_name = "min(" + base.esc_name + ")";
    ProglangType ptype = base.type.elementType();
    ProglangType rtype = base.rep_type.elementType();
    VarComparability comp = base.comparability.elementType();
    return new VarInfo(name, esc_name, ptype, rtype, comp);
  }

}
