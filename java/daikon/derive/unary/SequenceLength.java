package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import utilMDE.*;

// originally from pass1.
public final class SequenceLength
  extends UnaryDerivation
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /**
   * Boolean.  True iff SequenceLength derived variables should be generated.
   **/
  public static boolean dkconfig_enabled = true;

  public final int shift;

  public SequenceLength(VarInfo vi, int shift) {
    super(vi);
    this.shift = shift;         // typically 0 or -1
  }

  public static boolean applicable(VarInfo vi) {
    Assert.assert(vi.rep_type.isArray());

    if (vi.derived != null) {
      Assert.assert((vi.derived instanceof SequenceScalarSubsequence) ||
                    (vi.derived instanceof SequenceStringIntersection) ||
                    (vi.derived instanceof SequenceScalarIntersection) ||
                    (vi.derived instanceof SequenceStringUnion) ||
                    (vi.derived instanceof SequenceScalarUnion) ||
                    (vi.derived instanceof SequencesConcat)      ||
                    (vi.derived instanceof SequencesJoin)
                    );

      if (
          (vi.derived instanceof SequenceStringIntersection) ||
          (vi.derived instanceof SequenceScalarIntersection) ||
          (vi.derived instanceof SequenceStringUnion) ||
          (vi.derived instanceof SequenceScalarUnion)
          )

        return true;
      else
        return false;
    }
    // Don't do this for now, because we depend on being able to call
    // sequenceSize() later.
    // if (vi.name.indexOf("~.") != -1)
    //   return false;

    return true;
  }

  public ValueAndModified computeValueAndModified(ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING)
      return ValueAndModified.MISSING;
    Object val = base.getValue(vt);
    if (val == null) {
      return ValueAndModified.MISSING;
    }

    int len;
    ProglangType rep_type = base.rep_type;

    if (rep_type == ProglangType.INT_ARRAY) {
      len = ((long[])val).length;
    } else if (rep_type == ProglangType.DOUBLE_ARRAY) {
      len = ((double[])val).length;
    } else {
      len = ((Object[])val).length;
    }
    return new ValueAndModified(Intern.internedLong(len+shift), source_mod);
  }

  protected VarInfo makeVarInfo() {
    VarInfoName name = base.name.applySize();
    switch (shift) {
    case 0:
      break;
    case -1:
      name = name.applyDecrement();
      break;
    default:
      throw new UnsupportedOperationException("Unsupported shift: " + shift);
    }
    ProglangType ptype = ProglangType.INT;
    ProglangType frtype = ProglangType.INT;
    VarComparability comp = base.comparability.indexType(0);
    return new VarInfo(name, ptype, frtype, comp, VarInfoAux.getDefault());
  }

  public  boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceLength)
      && (((SequenceLength) other).shift == this.shift);
  }

}
