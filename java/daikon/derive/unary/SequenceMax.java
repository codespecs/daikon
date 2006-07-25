package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

// like SequenceMin; if one changes, change the other, too
public final class SequenceMax
  extends UnaryDerivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff SequencesMax derived variables should be generated.
   **/
  public static boolean dkconfig_enabled = false;

  public SequenceMax(VarInfo vi) {
    super(vi);
  }

  public ValueAndModified computeValueAndModifiedImpl(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING_NONSENSICAL)
      return ValueAndModified.MISSING_NONSENSICAL;
    Object val = base.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING_NONSENSICAL;
    if (val instanceof long[]) {
      long[] val_array = (long[])val;
      if (val_array.length == 0)
        return ValueAndModified.MISSING_NONSENSICAL;
      return new ValueAndModified(Intern.internedLong(ArraysMDE.max(val_array)),
                                  source_mod);
    } else if (val instanceof double[]) {
      double[] val_array = (double[])val;
      if (val_array.length == 0)
        return ValueAndModified.MISSING_NONSENSICAL;
      return new ValueAndModified(Intern.internedDouble(ArraysMDE.max(val_array)),
                                  source_mod);
    } else {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
  }

  protected VarInfo makeVarInfo() {
    VarInfoName viname = base.name.applyFunction("max");
    ProglangType ptype = base.type.elementType();
    ProglangType frtype = base.file_rep_type.elementType();
    VarComparability comp = base.comparability.elementType();
    VarInfo vi = new VarInfo(viname, ptype, frtype, comp, base.aux);

    vi.setup_derived_base (base);
    vi.var_kind = VarInfo.VarKind.FUNCTION;
    vi.enclosing_var = base;
    vi.arr_dims = 0;
    vi.function_args = null;
    vi.relative_name = "max";
    if ((vi.parent_ppt != null) && (vi.parent_variable != null)) {
      vi.parent_variable = String.format ("max(%s)", base.parent_variable);
    }
    return (vi);
  }

  public  boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceMax);
  }

}
