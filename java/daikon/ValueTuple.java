package daikon;

import daikon.derive.*;

import org.apache.log4j.Category;

import java.util.*;

import utilMDE.*;


/**
 * This is the data structure that holds the tuples of values seen so far
 * (and how many times each was seen) for a particular program point.  VarInfo
 * objects can use this to get the values of the variables they represent.
 * <br>
 * While the arrays and their elements are interned, the ValueTuple objects
 * themselves are not interned.
 **/
public final class ValueTuple implements Cloneable {

  /** Debug tracer **/
  public static Category debug = Category.getInstance ("daikon.ValueTuple");

  // These arrays are interned, and so are their elements.

  public Object[] vals;         // the values themselves (as Objects, if necessary)

  // consider putting this in the first slot of "vals", to avoid the Object
  // overhead of a pair of val and mods.  Do I need to worry about trickery
  // such as orderings changing when we add derived values?  I think not...

  // I need to have some kind of access to this representation so that
  // external code can create one of these and pass it in.  Or maybe
  // external code always passes in an ordinary array and I convert it to
  // the packed representation if appropriate.  (That does seem cleaner,
  // although it might be less efficient.)

  public int[] mods;            // modification bit per value, possibly packed
                                // into fewer ints than the vals above.
                                // Don't use a single int because that
                                // won't scale to (say) more than 16
                                // values.


  // Right now there are only three meaningful values for a mod:
  public final static int UNMODIFIED = 0;
  public final static int MODIFIED = 1;
  public final static int MISSING = 2;
  public final static int MODBIT_VALUES = 3;
  // Out of the range of MODBIT_VALUES because this won't appear in the
  // tables; it gets converted to UNMODIFIED or MODIFIED, depending on
  // whether this is the first sample.  (Not sure whether that is the right
  // strategy in the long term; it does let me avoid changing code in the
  // short term.)
  public final static int STATIC_CONSTANT = 22;

  // implementation for unpacked representation

  int getModified(VarInfo vi) { return vi.getModified(this); }
  boolean isUnmodified(VarInfo vi) { return vi.isUnmodified(this); }
  boolean isModified(VarInfo vi) { return vi.isModified(this); }
  boolean isMissing(VarInfo vi) { return vi.isMissing(this); }

  int getModified(int value_index) { return mods[value_index]; }
  boolean isUnmodified(int value_index) { return mods[value_index] == UNMODIFIED; }
  boolean isModified(int value_index) { return mods[value_index] == MODIFIED; }
  boolean isMissing(int value_index) { return mods[value_index] == MISSING; }

  // The arguments ints represent modification information.
  static boolean modIsUnmodified(int mod_value) { return mod_value == UNMODIFIED; }
  static boolean modIsModified(int mod_value) { return mod_value == MODIFIED; }
  static boolean modIsMissing(int mod_value) { return mod_value == MISSING; }

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

  public final static int TUPLEMOD_VALUES = MathMDE.pow(2, MODBIT_VALUES);
  public final static int UNMODIFIED_BITVAL = MathMDE.pow(2, UNMODIFIED);
  public final static int MODIFIED_BITVAL = MathMDE.pow(2, MODIFIED);
  public final static int MISSING_BITVAL = MathMDE.pow(2, MISSING);
  // Various slices of the 8 (=TUPLEMOD_VALUES) possible tuplemod values.
  // The arrays are filled up in a static block below.
  // (As of 1/9/2000, tuplemod_modified_not_missing is used only in
  // num_mod_non_missing_samples(), and tuplemod_not_missing is not used.)
  public final static int[] tuplemod_not_missing = new int[TUPLEMOD_VALUES/2];
  public final static int[] tuplemod_modified_not_missing = new int[TUPLEMOD_VALUES/4];

  static {
    int i1 = 0, i2 = 0;
    for (int tm=0; tm<TUPLEMOD_VALUES; tm++) {
      if (!tuplemodHasMissing(tm)) {
        tuplemod_not_missing[i1] = tm;
        i1++;
      }
      if (tuplemodHasModified(tm) && !tuplemodHasMissing(tm)) {
        tuplemod_modified_not_missing[i2] = tm;
        i2++;
      }
    }
  }

  static int make_tuplemod(boolean unmodified, boolean modified, boolean missing) {
    int result = 0;
    if (unmodified) result += UNMODIFIED_BITVAL;
    if (modified) result += MODIFIED_BITVAL;
    if (missing) result += MISSING_BITVAL;
    return result;
  }

  static boolean tuplemodHasModified(int tuplemod) {
    return ((tuplemod & MODIFIED_BITVAL) != 0);
  }
  static boolean tuplemodHasUnmodified(int tuplemod) {
    return ((tuplemod & UNMODIFIED_BITVAL) != 0);
  }
  static boolean tuplemodHasMissing(int tuplemod) {
    return ((tuplemod & MISSING_BITVAL) != 0);
  }

  /**
   * In output, M=modified, U=unmodified, X=missing.
   * Capital letters indicate the specified modbit does occur,
   * lowercase letters indicate it does not occur.
   **/
  static String tuplemodToStringBrief(int tuplemod) {
    return ((tuplemodHasModified(tuplemod) ? "M" : "m")
            + (tuplemodHasUnmodified(tuplemod) ? "U" : "u")
            + (tuplemodHasMissing(tuplemod) ? "X" : "x"));
  }


  static int tupleMod(int[] mods) {
    boolean[] has_modbit_val = new boolean[MODBIT_VALUES];
    // Extraneous, as the array is initialized to all zeroes.
    Arrays.fill(has_modbit_val, false);
    for (int i=0; i<mods.length; i++) {
      has_modbit_val[mods[i]] = true;
    }
    return make_tuplemod(has_modbit_val[UNMODIFIED],
                         has_modbit_val[MODIFIED],
                         has_modbit_val[MISSING]);
  }

  int tupleMod() {
    return ValueTuple.tupleMod(mods);
  }

  static int parseModified(String raw) {
    int result = Integer.parseInt(raw);
    Assert.assertTrue((result >= 0) && (result < MODBIT_VALUES));
    return result;
  }


  /**
   * Get the value of the variable vi in this ValueTuple.  Note: the
   * VarInfo form is preferred
   * @param vi the variable whose value is to be returned
   * @return the value of the variable at this ValueTuple
   **/
  Object getValue(VarInfo vi) { return vi.getValue(this); }

  /**
   * Get the value at the val_index.
   * Note: For clients, getValue(VarInfo) is preferred to getValue(int).
   * @see #getValue(VarInfo)
   **/
  Object getValue(int val_index) { return vals[val_index]; }


  /** Default constructor that interns its argument. */
  ValueTuple(Object[] vals, int[] mods) {
    for (int i=0; i<vals.length; i++)
      Assert.assertTrue(Intern.isInterned(vals[i]));
    this.vals = Intern.intern(vals);
    this.mods = Intern.intern(mods);
  }

  // Private constructor that doesn't perform interning.
  private ValueTuple(Object[] vals, int[] mods, boolean check) {
    Assert.assertTrue((!check) || Intern.isInterned(vals));
    Assert.assertTrue((!check) || Intern.isInterned(mods));
    this.vals = vals;
    this.mods = mods;
  }

  /**
   * More convenient name for the constructor that doesn't intern.
   *
   * This is not private because it is used (only) by read_data_trace_file,
   * which makes a partial ValueTuple, fills it in with derived variables,
   * and only then interns it; the alternative would be for derived
   * variables to take separate vals and mods arguments.  No one else
   * should use it!
   **/
  public static ValueTuple makeUninterned(Object[] vals, int[] mods) {
    return new ValueTuple(vals, mods, false);
  }


  // Do I need/want this?  Probably in some circumstances...  But in
  // general, users should use the constructor.
  /** Constructor that takes already-interned arguments. */
  static ValueTuple makeFromInterned(Object[] vals, int[] mods) {
    return new ValueTuple(vals, mods, true);
  }


  // Like clone(), but avoids its problems of default access and returning
  // an Object.
  public ValueTuple shallowcopy() {
    return ValueTuple.makeFromInterned(vals, mods);
  }

  // public Object clone() {
  //   return ValueTuple.makeFromInterned(vals, mods);
  // }


  // These definitions are intended to make different ValueTuples with the
  // same contents compare identically.
  public boolean equals(Object obj) {
    if (! (obj instanceof ValueTuple))
      return false;
    ValueTuple other = (ValueTuple) obj;
    return (vals == other.vals) && (mods == other.mods);
  }
  public int hashCode() {
    return vals.hashCode() * 31 + mods.hashCode();
  }


  public int size() {
    Assert.assertTrue(vals.length == mods.length);
    return vals.length;
  }

  // Return a new ValueTuple containing this one's first len elements.
  public ValueTuple trim(int len) {
    Object[] new_vals = ArraysMDE.subarray(vals, 0, len);
    int[] new_mods = ArraysMDE.subarray(mods, 0, len);
    return new ValueTuple(new_vals, new_mods);
  }


  // This modifies the ValueTuple in place!!
  // I think that's OK, because I only compare using equality and hashing
  // doesn't depend on the elements.  (The new subparts are properly interned.)
  void extend(Derivation[] derivs) {
    int old_len = vals.length;
    Object[] new_vals = new Object[old_len + derivs.length];
    System.arraycopy(vals, 0, new_vals, 0, old_len);
    int[] new_mods = new int[old_len + derivs.length];
    System.arraycopy(mods, 0, new_mods, 0, old_len);
    for (int i=0; i<derivs.length; i++) {
      Derivation deriv = derivs[i];
      // It might be slightly more efficient to pass in the mods and vals
      // arrays instead of the ValueTuple; then add
      // VarInfo.getModified(Object[]) and VarInfo.getValue(Object[]).
      ValueAndModified vm = deriv.computeValueAndModified(this);
      new_vals[i+old_len] = vm.value;
      new_mods[i+old_len] = vm.modified;
// [[INCR]] ....
      // if (vm == ValueAndModified.MISSING)
      //   deriv.getVarInfo().canBeMissing = true;
// .... [[INCR]]
    }
    vals = Intern.intern(new_vals);
    mods = Intern.intern(new_mods);
  }

  // For debugging
  public String toString() {
    StringBuffer sb = new StringBuffer("[");
    Assert.assertTrue(vals.length == mods.length);
    for (int i=0; i<vals.length; i++) {
      if (i>0)
        sb.append("; ");
      if (vals[i] instanceof String)
        sb.append("\"" + vals[i] + "\"");
      else if (vals[i] instanceof long[])
        sb.append(ArraysMDE.toString((long[])vals[i]));
      else if (vals[i] instanceof int[])
        // shouldn't reach this case -- should be long[], not int[]
        sb.append(ArraysMDE.toString((int[])vals[i]));
      else
        sb.append(vals[i]);
      sb.append(",");
      sb.append(mods[i]);
    }
    sb.append("]");
    return sb.toString();
  }

  public static String valsToString(Object[] vals) {
    StringBuffer sb = new StringBuffer("[");
    for (int i=0; i<vals.length; i++) {
      if (i>0)
        sb.append(", ");
      if (vals[i] instanceof long[])
        sb.append(ArraysMDE.toString((long[])vals[i]));
      else if (vals[i] instanceof int[])
        // shouldn't reach this case -- should be long[], not int[]
        sb.append(ArraysMDE.toString((int[])vals[i]));
      else
        sb.append(vals[i]);
    }
    sb.append("]");
    return sb.toString();
  }

  /** For each index i, do dest[i] = dest[i] or other[i]. */
  public static void orModsInto(int[] dest, int[] other) {
    Assert.assertTrue(dest.length == other.length);
    int len = dest.length;
    for (int i=0; i<len; i++)
      if ((dest[i] == UNMODIFIED) && (other[i] == MODIFIED))
        dest[i] = MODIFIED;
  }

  /**
   * Return a new ValueTuple consisting of the elements of this one with
   * indices listed in indices.
   **/
  public ValueTuple slice(int[] indices) {
    int new_len = indices.length;
    Object[] new_vals = new Object[new_len];
    int[] new_mods = new int[new_len];
    for (int i=0; i<new_len; i++) {
      new_vals[i] = vals[indices[i]];
      new_mods[i] = mods[indices[i]];
    }
    return new ValueTuple(new_vals, new_mods);
  }

}
