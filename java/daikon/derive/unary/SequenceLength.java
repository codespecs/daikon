package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import utilMDE.*;

// originally from pass1.
public class SequenceLength extends UnaryDerivation {

  public SequenceLength(VarInfo vi) {
    super(vi);
  }

  public static boolean applicable(VarInfo vi) {
    if (!vi.rep_type.isArray())
      return false;
    if (vi.derived != null) {
      Assert.assert(vi.derived instanceof SequenceScalarSubsequence);
      return false;
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
    else {
      int len;
      // Can't use "==" because ProglangType objecs are not interned.
      if (var_info.rep_type.equals(ProglangType.INT_ARRAY)) {
        len = ((int[])val).length;
      } else {
        len = ((Object[])val).length;
      }
      return new ValueAndModified(new Integer(len), source_mod);
    }
  }

  protected VarInfo makeVarInfo() {
    String name = "size(" + var_info.name + ")";
    ProglangType ptype = ProglangType.INT;
    ProglangType rtype = ProglangType.INT;
    ExplicitVarComparability comp = var_info.comparability.indexType(0);
    return new VarInfo(name, ptype, rtype, comp);
  }

}
