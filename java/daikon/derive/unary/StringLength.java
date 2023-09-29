package daikon.derive.unary;

import daikon.ProglangType;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.Intern;

/** Length of String variables. */
public final class StringLength extends UnaryDerivation {
  static final long serialVersionUID = 20061016L;

  /** Boolean. True iff StringLength derived variables should be generated. */
  public static boolean dkconfig_enabled = false;

  public StringLength(VarInfo vi) {
    super(vi);
  }

  public static boolean applicable(VarInfo vi) {
    assert vi.rep_type.isString();
    return true;
  }

  @Override
  public ValueAndModified computeValueAndModifiedImpl(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING_NONSENSICAL) {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
    Object val = base.getValue(vt);
    if (val == null) {
      return ValueAndModified.MISSING_NONSENSICAL;
    }

    int len = ((String) val).length();
    return new ValueAndModified(Intern.internedLong(len), source_mod);
  }

  @Override
  protected VarInfo makeVarInfo() {
    return VarInfo.make_scalar_str_func("length", ProglangType.INT, base);
  }

  @Pure
  @Override
  public boolean isSameFormula(Derivation other) {
    return (other instanceof StringLength);
  }

  @Override
  @SideEffectFree
  public String csharp_name(String index) {
    return String.format("%s.Length", base.csharp_name());
  }

  @Override
  @SideEffectFree
  public String esc_name(String index) {
    return String.format("%s.length()", base.esc_name());
  }

  @Override
  public String jml_name(String index) {
    return String.format("%s.length()", base.jml_name());
  }

  @Override
  @SideEffectFree
  public String simplify_name() {
    return String.format("(stringLength %s)", base.simplify_name());
  }
}
