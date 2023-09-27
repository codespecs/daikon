package daikon.derive.binary;

import daikon.ProglangType;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.derive.Derivation;
import daikon.derive.ValueAndModified;
import java.util.logging.Logger;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.Intern;

/**
 * Represents the concatenation of two base variables. This derived variable works for both
 * sequences of numbers and strings.
 */
public final class SequencesConcat extends BinaryDerivation {
  static final long serialVersionUID = 20020122L;

  /** Debug tracer. */
  public static final Logger debug = Logger.getLogger("daikon.derive.binary.SequencesConcat");

  // Variables starting with dkconfig_ should only be set via the
  // daikon.config.Configuration interface.
  /** Boolean. True iff SequencesConcat derived variables should be created. */
  public static boolean dkconfig_enabled = false;

  @Override
  public VarInfo var1(@GuardSatisfied SequencesConcat this) {
    return base1;
  }

  @Override
  public VarInfo var2(@GuardSatisfied SequencesConcat this) {
    return base2;
  }

  /**
   * Create a new SequenceScarlarConcat that represents the concatenation of two base variables.
   *
   * @param vi1 base variable 1
   * @param vi2 base variable 2
   */
  public SequencesConcat(VarInfo vi1, VarInfo vi2) {
    super(vi1, vi2);
  }

  @Override
  public ValueAndModified computeValueAndModifiedImpl(ValueTuple full_vt) {
    Object val1 = var1().getValue(full_vt);
    Object val2 = var2().getValue(full_vt);

    int mod = ValueTuple.UNMODIFIED;
    int mod1 = base1.getModified(full_vt);
    int mod2 = base2.getModified(full_vt);

    if (mod1 == ValueTuple.MODIFIED) {
      mod = ValueTuple.MODIFIED;
    }
    if (mod1 == ValueTuple.MISSING_NONSENSICAL) {
      mod = ValueTuple.MISSING_NONSENSICAL;
    }
    if (mod2 == ValueTuple.MODIFIED) {
      mod = ValueTuple.MODIFIED;
    }
    if (mod2 == ValueTuple.MISSING_NONSENSICAL) {
      mod = ValueTuple.MISSING_NONSENSICAL;
    }

    if (val1 == null && val2 == null) {
      return new ValueAndModified(null, mod);
    }
    if (var1().rep_type == ProglangType.INT_ARRAY) {
      // val1 instanceof long[] || val2 instanceof long[]
      long[] result =
          ArraysPlume.concat(
              val1 == null ? null : (long[]) val1, val2 == null ? null : (long[]) val2);
      return new ValueAndModified(Intern.intern(result), mod);
    } else if (var1().rep_type == ProglangType.DOUBLE_ARRAY) {
      double[] result =
          ArraysPlume.concat(
              val1 == null ? null : (double[]) val1, val2 == null ? null : (double[]) val2);
      return new ValueAndModified(Intern.intern(result), mod);

    } else if (var1().rep_type == ProglangType.STRING_ARRAY) {
      // val1 instanceof String[] || val2 instanceof String[]
      @Interned String[] result =
          ArraysPlume.concat(
              val1 == null ? null : (@Interned String[]) val1,
              val2 == null ? null : (@Interned String[]) val2);
      return new ValueAndModified(Intern.intern(result), mod);
    } else {
      throw new Error("Attempted to concatenate unknown arrays");
    }
  }

  @Override
  protected VarInfo makeVarInfo() {
    return VarInfo.make_function("concat", var1(), var2());
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied SequencesConcat this) {
    return "[SequencesConcat of " + var1().name() + " " + var2().name() + "]";
  }

  @Pure
  @Override
  public boolean isSameFormula(Derivation other) {
    return (other instanceof SequencesConcat);
  }

  @SideEffectFree
  @Override
  public String esc_name(String index) {
    return String.format("SequencesConcat[%s,%s]", var1().esc_name(), var2().esc_name());
  }
}
