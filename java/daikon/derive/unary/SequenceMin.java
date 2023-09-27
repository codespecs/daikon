package daikon.derive.unary;

import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.Intern;

// like SequenceMax; if one changes, change the other, too
public final class SequenceMin extends UnaryDerivation {
  static final long serialVersionUID = 20020122L;

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequenceMin derived variables should be generated. */
  public static boolean dkconfig_enabled = false;

  public SequenceMin(VarInfo vi) {
    super(vi);
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
    if (val instanceof long[]) {
      long[] val_array = (long[]) val;
      if (val_array.length == 0) {
        return ValueAndModified.MISSING_NONSENSICAL;
      }
      return new ValueAndModified(Intern.internedLong(ArraysPlume.min(val_array)), source_mod);
    } else if (val instanceof double[]) {
      double[] val_array = (double[]) val;
      if (val_array.length == 0) {
        return ValueAndModified.MISSING_NONSENSICAL;
      }
      return new ValueAndModified(Intern.internedDouble(ArraysPlume.min(val_array)), source_mod);

    } else {
      return ValueAndModified.MISSING_NONSENSICAL;
    }
  }

  @Override
  protected VarInfo makeVarInfo() {
    return VarInfo.make_scalar_seq_func("min", null, base, 0);
  }

  @Pure
  @Override
  public boolean isSameFormula(Derivation other) {
    return (other instanceof SequenceMin);
  }

  @SideEffectFree
  @Override
  public String esc_name(String index) {
    return String.format("min(%s)", base.esc_name());
  }
}
