package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import utilMDE.*;

// like SequenceMax; if one changes, change the other, too
public final class SequenceMin
  extends UnaryDerivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff SequenceMin derived variables should be generated.
   **/
  public static boolean dkconfig_enabled = false;

  public SequenceMin(VarInfo vi) {
    super(vi);
  }

  public ValueAndModified computeValueAndModifiedImpl(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    Object val = base.getValue(vt);
    if (val == null)
      return ValueAndModified.MISSING_NONSENSICAL;
    if (val instanceof long[]) {
      long[] val_array = (long[])val;
      if (val_array.length == 0)
        return ValueAndModified.MISSING_NONSENSICAL;
      return new ValueAndModified(Intern.internedLong(ArraysMDE.min(val_array)),
                                  source_mod);
    } else if (val instanceof double[]) {
      double[] val_array = (double[])val;
      if (val_array.length == 0)
        return ValueAndModified.MISSING_NONSENSICAL;
      return new ValueAndModified(Intern.internedDouble(ArraysMDE.min(val_array)),
                                  source_mod);

    } else {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
  }

  protected VarInfo makeVarInfo() {
    VarInfoName name = base.name.applyFunction("min");
    ProglangType ptype = base.type.elementType();
    ProglangType frtype = base.file_rep_type.elementType();
    VarComparability comp = base.comparability.elementType();
    return new VarInfo(name, ptype, frtype, comp, base.aux);
  }

  public  boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceMin);
  }

}
