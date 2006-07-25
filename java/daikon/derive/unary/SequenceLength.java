package daikon.derive.unary;
import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import daikon.derive.ternary.*;
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
    Assert.assertTrue(vi.rep_type.isArray());

    if (vi.derived != null) {
      Assert.assertTrue
        ((vi.derived instanceof SequenceScalarSubsequence)
         || (vi.derived instanceof SequenceScalarArbitrarySubsequence)
         || (vi.derived instanceof SequenceStringIntersection)
         || (vi.derived instanceof SequenceScalarIntersection)
         || (vi.derived instanceof SequenceStringUnion)
         || (vi.derived instanceof SequenceScalarUnion)
         || (vi.derived instanceof SequencesConcat)
         || (vi.derived instanceof SequencesPredicate)
         || (vi.derived instanceof SequencesJoin)
         || (vi.derived instanceof SequenceFloatSubsequence)
         || (vi.derived instanceof SequenceFloatArbitrarySubsequence)
         || (vi.derived instanceof SequenceFloatIntersection)
         || (vi.derived instanceof SequenceFloatUnion)
         || (vi.derived instanceof SequencesPredicateFloat)
         || (vi.derived instanceof SequencesJoinFloat)
         );

      if (!( // All of the below give new information when taking a sizeof
            (vi.derived instanceof SequenceStringIntersection)
            || (vi.derived instanceof SequenceScalarIntersection)
            || (vi.derived instanceof SequenceStringUnion)
            || (vi.derived instanceof SequenceScalarUnion)
            || (vi.derived instanceof SequencesConcat)
            || (vi.derived instanceof SequenceFloatIntersection)
            || (vi.derived instanceof SequenceFloatUnion)

            )) {
        return false;
      }
    }
    // Don't do this for now, because we depend on being able to call
    // sequenceSize() later.
    // if (vi.name.indexOf("~.") != -1)
    //   return false;

    return true;
  }

  public ValueAndModified computeValueAndModifiedImpl (ValueTuple vt) {
    int source_mod = base.getModified(vt);
    if (source_mod == ValueTuple.MISSING_NONSENSICAL)
      return ValueAndModified.MISSING_NONSENSICAL;
    Object val = base.getValue(vt);
    if (val == null) {
      return ValueAndModified.MISSING_NONSENSICAL;
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
    VarInfoName viname = base.name.applySize();
    switch (shift) {
    case 0:
      break;
    case -1:
      viname = viname.applyDecrement();
      break;
    default:
      throw new UnsupportedOperationException("Unsupported shift: " + shift);
    }
    ProglangType ptype = ProglangType.INT;
    ProglangType frtype = ProglangType.INT;
    VarComparability comp = base.comparability.indexType(0);
    VarInfo vi = new VarInfo(viname, ptype, frtype, comp,
                             VarInfoAux.getDefault());
    vi.setup_derived_base (base);
    vi.var_kind = VarInfo.VarKind.FUNCTION;
    vi.enclosing_var = base;
    vi.arr_dims = 0;
    vi.function_args = null;
    vi.relative_name = "size" + ((shift == -1) ? "_minus1" : "");
    if (vi.parent_ppt != null) {
      if (base.parent_variable == null)
        vi.parent_variable = null;
      else {
        vi.parent_variable = String.format ("size(%s)%s", base.parent_variable,
                                            ((shift == -1) ? "-1" : ""));
      }
    }
    return (vi);
  }

  public  boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceLength)
      && (((SequenceLength) other).shift == this.shift);
  }

}
