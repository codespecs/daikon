package daikon.derive.unary;

import daikon.*;
import daikon.derive.*;
import daikon.derive.binary.*;
import daikon.derive.ternary.*;
import plume.*;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

// originally from pass1.
public final class SequenceLength extends UnaryDerivation {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequenceLength derived variables should be generated. */
  public static boolean dkconfig_enabled = true;

  public final int shift;

  public SequenceLength(VarInfo vi, int shift) {
    super(vi);
    this.shift = shift; // typically 0 or -1
  }

  public static boolean applicable(VarInfo vi) {
    assert vi.rep_type.isArray();

    if (vi.derived != null) {
      assert ((vi.derived instanceof SequenceScalarSubsequence)
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
          || (vi.derived instanceof SequencesJoinFloat));

      if (!( // All of the below give new information when taking a sizeof
      (vi.derived instanceof SequenceStringIntersection)
          || (vi.derived instanceof SequenceScalarIntersection)
          || (vi.derived instanceof SequenceStringUnion)
          || (vi.derived instanceof SequenceScalarUnion)
          || (vi.derived instanceof SequencesConcat)
          || (vi.derived instanceof SequenceFloatIntersection)
          || (vi.derived instanceof SequenceFloatUnion))) {

        return false;
      }
    }
    // Don't do this for now, because we depend on being able to call
    // sequenceSize() later.
    // if (vi.name.indexOf("~.") != -1)
    //   return false;

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

    int len;
    ProglangType rep_type = base.rep_type;

    if (rep_type == ProglangType.INT_ARRAY) {
      len = ((long[]) val).length;
    } else if (rep_type == ProglangType.DOUBLE_ARRAY) {
      len = ((double[]) val).length;
    } else {
      len = ((Object[]) val).length;
    }
    return new ValueAndModified(Intern.internedLong(len + shift), source_mod);
  }

  @Override
  @SuppressWarnings("keyfor") // need EnsuresQualifier feature
  protected VarInfo makeVarInfo() {
    VarInfo v = VarInfo.make_scalar_seq_func("size", ProglangType.INT, base, shift);

    if (base.aux.hasValue(VarInfoAux.MINIMUM_LENGTH)) {
      v.aux =
          v.aux.setInt(
              VarInfoAux.MINIMUM_VALUE, base.aux.getInt(VarInfoAux.MINIMUM_LENGTH) + shift);
    }
    if (base.aux.hasValue(VarInfoAux.MAXIMUM_LENGTH)) {
      v.aux =
          v.aux.setInt(
              VarInfoAux.MAXIMUM_VALUE, base.aux.getInt(VarInfoAux.MAXIMUM_LENGTH) + shift);
    }

    return v;
  }

  /*@Pure*/
  @Override
  public boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceLength) && (((SequenceLength) other).shift == this.shift);
  }

  /** Returns the ESC name */
  @Override
  @SuppressWarnings("nullness")
  /*@SideEffectFree*/
  public String esc_name(String index) {
    // This should be able to use Quantify.Length to calculate the name,
    // but it can't because the old version formatted these slightly
    // differently.  But this could be used when the old regression results
    // are no longer needed.
    // Quantify.Length  ql = new Quantify.Length (base, shift);
    // return ql.esc_name();

    if (base.isPrestate()) {
      return String.format(
          "\\old(%s.length)%s", base.enclosing_var.postState.esc_name(), shift_str(shift));
    } else {
      return String.format("%s.length%s", base.enclosing_var.esc_name(), shift_str(shift));
    }
  }

  /** Returns the JML name */
  @Override
  public String jml_name(String index) {
    Quantify.Length ql = new Quantify.Length(base, shift);
    return ql.jml_name();
  }

  /** Returns the Simplify name */
  /*@SideEffectFree*/
  @Override
  public String simplify_name() {
    Quantify.Length ql = new Quantify.Length(base, shift);
    return ql.simplify_name();
  }

  /** Returns the CSharpContract name */
  /*@SideEffectFree*/
  @Override
  public String csharp_name(String index) {
    Quantify.Length ql = new Quantify.Length(base, shift);
    return ql.csharp_name();
  }

  /** Adds one to the default complexity if shift is not 0 */
  @Override
  public int complexity() {
    return super.complexity() + ((shift != 0) ? 1 : 0);
  }
}
