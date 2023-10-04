package daikon;

import java.util.Arrays;
import java.util.StringJoiner;
import java.util.logging.Logger;
import org.checkerframework.checker.initialization.qual.UnknownInitialization;
import org.checkerframework.checker.interning.qual.Interned;
import org.checkerframework.checker.lock.qual.GuardSatisfied;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.checkerframework.dataflow.qual.SideEffectFree;
import org.plumelib.util.ArraysPlume;
import org.plumelib.util.Intern;
import org.plumelib.util.MathPlume;

/**
 * This data structure holds a tuple of values for a particular program point. VarInfo objects can
 * use this to get the values of the variables they represent.
 *
 * <p>It has two fields: vals and mods. While the arrays and their elements are interned, the
 * ValueTuple objects themselves are not interned.
 */
public final class ValueTuple implements Cloneable {

  /** Debug tracer. */
  public static Logger debug = Logger.getLogger("daikon.ValueTuple");

  // These arrays are interned, and so are their elements.
  // Each element is null only if it is missing (according to the mods array).
  public @Nullable @Interned Object @Interned [] vals;

  // Could consider putting the mods array in the first slot of "vals", to
  // avoid the Object overhead of a pair of val and mods.

  /**
   * Modification bit per value, possibly packed into fewer ints than the vals field. Don't use a
   * single int because that won't scale to (say) more than 32 values.
   */
  public int @Interned [] mods;

  // Right now there are only three meaningful values for a mod:
  /** Not modified. */
  public static final int UNMODIFIED = 0;

  /** Modified. */
  public static final int MODIFIED = 1;

  /**
   * Missing value because the expression doesn't make sense: x.a when x is null. Data trace files
   * can contain this modbit.
   */
  public static final int MISSING_NONSENSICAL = 2;

  /**
   * Missing value because of data flow: this.x.x isn't available from a ppt. Data trace files must
   * not contain this modbit.
   */
  public static final int MISSING_FLOW = 3;

  /** Maximum mod bit value. Always set to 1+ last modbit value. */
  public static final int MODBIT_VALUES = 4;

  /**
   * Out of the range of MODBIT_VALUES because this won't appear in the tables; it gets converted to
   * UNMODIFIED or MODIFIED, depending on whether this is the first sample. (Not sure whether that
   * is the right strategy in the long term; it does let me avoid changing code in the short term.)
   */
  public static final int STATIC_CONSTANT = 22;

  // Implementation for unpacked representation.
  // (An alternate representation would pack the mod values into fewer ints
  // than the vals field.)

  @Pure
  public int getModified(VarInfo vi) {
    return vi.getModified(this);
  }

  @Pure
  public boolean isUnmodified(VarInfo vi) {
    return vi.isUnmodified(this);
  }

  @Pure
  public boolean isModified(VarInfo vi) {
    return vi.isModified(this);
  }

  @Pure
  public boolean isMissingNonsensical(VarInfo vi) {
    return vi.isMissingNonsensical(this);
  }

  @Pure
  public boolean isMissingFlow(VarInfo vi) {
    return vi.isMissingFlow(this);
  }

  @SuppressWarnings("nullness") // postcondition: array expression
  @EnsuresNonNullIf(result = false, expression = "vals[#1.value_index]")
  @Pure
  public boolean isMissing(VarInfo vi) {
    return vi.isMissing(this);
  }

  @Pure
  int getModified(int value_index) {
    return mods[value_index];
  }

  @Pure
  boolean isUnmodified(int value_index) {
    return mods[value_index] == UNMODIFIED;
  }

  @Pure
  boolean isModified(int value_index) {
    return mods[value_index] == MODIFIED;
  }

  @Pure
  boolean isMissingNonsensical(
      @UnknownInitialization(ValueTuple.class) ValueTuple this, int value_index) {
    return mods[value_index] == MISSING_NONSENSICAL;
  }

  @Pure
  boolean isMissingFlow(@UnknownInitialization(ValueTuple.class) ValueTuple this, int value_index) {
    return mods[value_index] == MISSING_FLOW;
  }

  /**
   * Returns true if the value at the given index is missing.
   *
   * @param value_index an index into this ValueTuple
   * @return true if the value at the given index is missing
   */
  @SuppressWarnings(
      "nullness:contracts.conditional.postcondition" // dependent: vals[i] is non-null if mods[i] !=
  // MISSING_*
  )
  @EnsuresNonNullIf(result = false, expression = "vals[#1]")
  @Pure
  boolean isMissing(@UnknownInitialization(ValueTuple.class) ValueTuple this, int value_index) {
    return isMissingNonsensical(value_index) || isMissingFlow(value_index);
  }

  // The arguments ints represent modification information.
  @Pure
  static boolean modIsUnmodified(int mod_value) {
    return mod_value == UNMODIFIED;
  }

  @Pure
  static boolean modIsModified(int mod_value) {
    return mod_value == MODIFIED;
  }

  @Pure
  static boolean modIsMissingNonsensical(int mod_value) {
    return mod_value == MISSING_NONSENSICAL;
  }

  @Pure
  static boolean modIsMissingFlow(int mod_value) {
    return mod_value == MISSING_FLOW;
  }

  // A tuplemod is summary modification information about the whole tuple
  // rather than about specific elements of the tuple.
  // There are two potentially useful abstractions for mod bits over an
  // entire tuple in aggregate:
  //  * return missing if any is missing (good for slices;
  //    indicates that we can't use that value)
  //  * return missing is all are missing (good for non-slices;
  //    the number that are guaranteed to be missing in slices
  // The same thing can be argued about "unmodified", actually.
  // So there are 8 states corresponding to 3 booleans:
  // modified, unmodified, missing
  //  * has modified, has unmodified, has missing
  //  * has modified, has unmodified, no missing
  //  * has modified, no unmodified, has missing
  //  * has modified, no unmodified, no missing
  //    ie, all modified
  //  * no modified, has unmodified, has missing
  //  * no modified, has unmodified, no missing
  //    ie, all unmodified
  //  * no modified, no unmodified, has missing
  //    ie, all missing; probably impossible
  //  * no modified, no unmodified, no missing
  //    impossible

  public static final int TUPLEMOD_VALUES = MathPlume.pow(2, MODBIT_VALUES);
  public static final int UNMODIFIED_BITVAL = MathPlume.pow(2, UNMODIFIED);
  public static final int MODIFIED_BITVAL = MathPlume.pow(2, MODIFIED);
  public static final int MISSING_NONSENSICAL_BITVAL = MathPlume.pow(2, MISSING_NONSENSICAL);
  public static final int MISSING_FLOW_BITVAL = MathPlume.pow(2, MISSING_FLOW);

  // Various slices of the 8 (=TUPLEMOD_VALUES) possible tuplemod values.
  // The arrays are filled up in a static block below.
  // (As of 1/9/2000, tuplemod_modified_not_missing is used only in
  // num_mod_samples(), and tuplemod_not_missing is not used.)
  public static final int[] tuplemod_not_missing = new int[TUPLEMOD_VALUES / 2];
  public static final int[] tuplemod_modified_not_missing = new int[TUPLEMOD_VALUES / 4];

  static {
    int i1 = 0, i2 = 0;
    for (int tm = 0; tm < TUPLEMOD_VALUES; tm++) {
      if (!tuplemodHasMissingFlow(tm) && !tuplemodHasMissingNonsensical(tm)) {
        tuplemod_not_missing[i1] = tm;
        i1++;
      }
      if (tuplemodHasModified(tm)
          && !tuplemodHasMissingFlow(tm)
          && !tuplemodHasMissingNonsensical(tm)) {
        tuplemod_modified_not_missing[i2] = tm;
        i2++;
      }
    }
  }

  static int make_tuplemod(
      boolean unmodified, boolean modified, boolean missingNonsensical, boolean missingFlow) {
    int result = 0;
    if (unmodified) {
      result += UNMODIFIED_BITVAL;
    }
    if (modified) {
      result += MODIFIED_BITVAL;
    }
    if (missingNonsensical) {
      result += MISSING_NONSENSICAL_BITVAL;
    }
    if (missingFlow) {
      result += MISSING_FLOW_BITVAL;
    }
    return result;
  }

  static boolean tuplemodHasModified(int tuplemod) {
    return ((tuplemod & MODIFIED_BITVAL) != 0);
  }

  static boolean tuplemodHasUnmodified(int tuplemod) {
    return ((tuplemod & UNMODIFIED_BITVAL) != 0);
  }

  static boolean tuplemodHasMissingNonsensical(int tuplemod) {
    return ((tuplemod & MISSING_NONSENSICAL_BITVAL) != 0);
  }

  static boolean tuplemodHasMissingFlow(int tuplemod) {
    return ((tuplemod & MISSING_FLOW_BITVAL) != 0);
  }

  /**
   * In output, M=modified, U=unmodified, X=missing. Capital letters indicate the specified modbit
   * does occur, lowercase letters indicate it does not occur.
   */
  static String tuplemodToStringBrief(int tuplemod) {
    return ((tuplemodHasModified(tuplemod) ? "M" : "m")
        + (tuplemodHasUnmodified(tuplemod) ? "U" : "u")
        + (tuplemodHasMissingNonsensical(tuplemod) ? "X" : "x")
        + (tuplemodHasMissingFlow(tuplemod) ? "F" : "f"));
  }

  static int tupleMod(int[] mods) {
    boolean[] has_modbit_val = new boolean[MODBIT_VALUES];
    // Extraneous, as the array is initialized to all zeroes.
    Arrays.fill(has_modbit_val, false);
    for (int i = 0; i < mods.length; i++) {
      has_modbit_val[mods[i]] = true;
    }
    return make_tuplemod(
        has_modbit_val[UNMODIFIED],
        has_modbit_val[MODIFIED],
        has_modbit_val[MISSING_NONSENSICAL],
        has_modbit_val[MISSING_FLOW]);
  }

  int tupleMod() {
    return ValueTuple.tupleMod(mods);
  }

  public static int parseModified(String raw) {
    int result = Integer.parseInt(raw);
    assert (result >= 0) && (result < MODBIT_VALUES);
    return result;
  }

  /**
   * Get the value of the variable vi in this ValueTuple.
   *
   * @param vi the variable whose value is to be returned
   * @return the value of the variable at this ValueTuple
   */
  public @Interned Object getValue(VarInfo vi) {
    assert vi.value_index < vals.length : vi;
    return vi.getValue(this);
  }

  /**
   * Get the value of the variable vi in this ValueTuple, or null if it is missing. Use of this
   * method is discouraged.
   *
   * @param vi the variable whose value is to be returned
   * @return the value of the variable at this ValueTuple
   * @see #getValue(VarInfo)
   */
  public @Nullable @Interned Object getValueOrNull(VarInfo vi) {
    assert vi.value_index < vals.length : vi;
    return vi.getValueOrNull(this);
  }

  /**
   * Get the value at the val_index, which should not have a missing value. Note: For clients,
   * getValue(VarInfo) is preferred to getValue(int).
   *
   * @see #getValue(VarInfo)
   */
  @Interned Object getValue(int val_index) {
    @SuppressWarnings("nullness") // context: precondition requires that the value isn't missing
    @NonNull Object result = vals[val_index];
    assert result != null;
    return result;
  }

  /**
   * Get the value at the val_index, or null if it is missing. Use of this method is (doubly)
   * discouraged.
   *
   * @see #getValue(int)
   */
  @Nullable @Interned Object getValueOrNull(int val_index) {
    Object result = vals[val_index];
    return result;
  }

  public void checkRep(@UnknownInitialization(ValueTuple.class) ValueTuple this) {
    assert vals.length == mods.length;
    for (int i = 0; i < vals.length; i++) {
      assert 0 <= mods[i] && mods[i] < MODBIT_VALUES
          : String.format("mods: %s i:%d mods[i]: %s%n", Arrays.toString(mods), i, mods[i]);
      assert (isMissing(i) ? vals[i] == null : true);
    }
  }

  /** Default constructor that interns its argument. */
  public ValueTuple(@Nullable @Interned Object[] vals, int[] mods) {
    this.vals = Intern.intern(vals);
    this.mods = Intern.intern(mods);
    checkRep();
  }

  /**
   * Private constructor that doesn't perform interning.
   *
   * @param vals the values
   * @param mods the modbits
   * @param check if true, require vals and mods to be interned
   */
  @SuppressWarnings("interning") // interning constructor
  private ValueTuple(@Nullable Object[] vals, int[] mods, boolean check) {
    assert !check || Intern.isInterned(vals);
    assert !check || Intern.isInterned(mods);
    this.vals = vals;
    this.mods = mods;
    checkRep();
  }

  /** Creates and returns a copy of this. */
  // Default implementation to quiet Findbugs.
  @SideEffectFree
  @Override
  public ValueTuple clone(@GuardSatisfied ValueTuple this) throws CloneNotSupportedException {
    return (ValueTuple) super.clone();
  }

  /**
   * More convenient name for the constructor that doesn't intern. That is, the result is an
   * <b>uninterned</b> ValueTuple.
   *
   * <p>This is not private because it is used (only) by read_data_trace_file, which makes a partial
   * ValueTuple, fills it in with derived variables, and only then interns it; the alternative would
   * be for derived variables to take separate vals and mods arguments. No one else should use it!
   */
  public static ValueTuple makeUninterned(@Nullable Object[] vals, int[] mods) {
    return new ValueTuple(vals, mods, false);
  }

  /** Constructor that takes already-interned arguments. */
  static ValueTuple makeFromInterned(@Nullable @Interned Object @Interned [] vals, int[] mods) {
    return new ValueTuple(vals, mods, true);
  }

  // Like clone(), but avoids its problems of default access and returning
  // an Object.
  public ValueTuple shallowcopy() {
    return ValueTuple.makeFromInterned(vals, mods);
  }

  // These definitions are intended to make different ValueTuples with the
  // same contents compare identically.
  @EnsuresNonNullIf(result = true, expression = "#1")
  @Pure
  @Override
  public boolean equals(@GuardSatisfied ValueTuple this, @GuardSatisfied @Nullable Object obj) {
    if (!(obj instanceof ValueTuple)) {
      return false;
    }
    ValueTuple other = (ValueTuple) obj;
    return (vals == other.vals) && (mods == other.mods);
  }

  @Pure
  @Override
  public int hashCode(@GuardSatisfied ValueTuple this) {
    return Arrays.hashCode(vals) * 31 + Arrays.hashCode(mods);
  }

  @Pure
  public int size() {
    assert vals.length == mods.length
        : String.format(
            "vals (len %d) = %s  mods (len %d = %s",
            vals.length, Arrays.toString(vals), mods.length, Arrays.toString(mods));
    return vals.length;
  }

  /** Return a new ValueTuple containing this one's first len elements. */
  public ValueTuple trim(int len) {
    @Nullable @Interned Object[] new_vals = ArraysPlume.subarray(vals, 0, len);
    int[] new_mods = ArraysPlume.subarray(mods, 0, len);
    return new ValueTuple(new_vals, new_mods);
  }

  @SideEffectFree
  @Override
  public String toString(@GuardSatisfied ValueTuple this) {
    return toString(null);
  }

  /**
   * Return the values of this tuple ("missing" is used for each missing value). If vis is non-null,
   * the values are annotated with the VarInfo name that would be associated with the value.
   */
  @SideEffectFree
  public String toString(@GuardSatisfied ValueTuple this, VarInfo @Nullable [] vis) {
    StringBuilder sb = new StringBuilder("[");
    assert vals.length == mods.length;
    assert vis == null || vals.length == vis.length;
    for (int i = 0; i < vals.length; i++) {
      if (i > 0) {
        sb.append("; ");
      }
      if (vis != null) {
        sb.append(vis[i].name() + "=");
      }
      Object val = vals[i];
      int mod = mods[i];
      switch (mod) {
        case UNMODIFIED:
        case MODIFIED:
          if (val instanceof String) {
            sb.append("\"" + val + "\"");
          } else if (val instanceof long[]) {
            sb.append(Arrays.toString((long[]) val));
          } else if (val instanceof int[]) {
            // shouldn't reach this case -- should be long[], not int[]
            // sb.append(Arrays.toString((int[])val));
            throw new Error("should be long[], not int[]");
          } else if (val instanceof double[]) {
            sb.append(Arrays.toString((double[]) val));
          } else if (val instanceof String[]) {
            sb.append(Arrays.toString((String[]) val));
          } else {
            sb.append(val);
          }
          if (mod == UNMODIFIED) {
            sb.append("(U)");
          }
          break;
        case MISSING_NONSENSICAL:
          sb.append("(missing)");
          break;
        case MISSING_FLOW:
          sb.append("(missing-flow)");
          break;
        default:
          throw new Error("bad modbit " + mod);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  public static String valsToString(@Nullable Object[] vals) {
    StringJoiner sj = new StringJoiner(", ", "[", "]");
    for (int i = 0; i < vals.length; i++) {
      sj.add(valToString(vals[i]));
    }
    return sj.toString();
  }

  public static String valToString(@Nullable Object val) {
    if (val == null) {
      return "null";
    }
    if (val instanceof long[]) {
      return Arrays.toString((long[]) val);
    } else if (val instanceof int[]) {
      // shouldn't reach this case -- should be long[], not int[]
      return Arrays.toString((int[]) val);
    } else {
      return val.toString();
    }
  }

  /**
   * Return a new ValueTuple consisting of the elements of this one with indices listed in indices.
   */
  public ValueTuple slice(int[] indices) {
    int new_len = indices.length;
    @Nullable @Interned Object[] new_vals = new @Nullable @Interned Object[new_len];
    int[] new_mods = new int[new_len];
    for (int i = 0; i < new_len; i++) {
      new_vals[i] = vals[indices[i]];
      new_mods[i] = mods[indices[i]];
    }
    return new ValueTuple(new_vals, new_mods);
  }
}
