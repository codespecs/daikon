package daikon;

import daikon.derive.*;

import utilMDE.*;


// This is the data structure that holds the tuples of values see so far
// (and how many times each was seen).

public class ValueTuple implements Cloneable {

  // These arrays should be interned.

  Object[] vals;		// the values themselves (as Objects, if necessary)

  // consider putting this in the first slot of "vals", to avoid having to
  // make a pair of val and mods.  Do I need to worry about trickery such
  // as orderings changing when we add derived values?  I think not...

  // I need to have some kind of access to this representation so that
  // external code can create one of these and pass it in.  Or maybe
  // external code always passes in an ordinary array and I convert it to
  // the packed representation if appropriate.  (That does seem cleaner,
  // although it might be less efficient.)

  private int[] mods;		// modification bit per value, possibly packed
				// into fewer ints than the vals above.
				// Don't use a single int because that
				// won't scale to (say) more than 16
				// values.


  // Right now there are only three meaningful values for a mod:
  public final static int UNMODIFIED = 0;
  public final static int MODIFIED = 1;
  public final static int MISSING = 2;
  public final static int MODBIT_VALUES = 3;

  // implementation for unpacked representation

  int getModified(VarInfo vi) { return vi.getModified(this); }
  boolean isUnmodified(VarInfo vi) { return vi.isUnmodified(this); }
  boolean isModified(VarInfo vi) { return vi.isModified(this); }
  boolean isMissing(VarInfo vi) { return vi.isMissing(this); }

  int getModified(int value_index) { return mods[value_index]; }
  boolean isUnmodified(int value_index) { return mods[value_index] == UNMODIFIED; }
  boolean isModified(int value_index) { return mods[value_index] == MODIFIED; }
  boolean isMissing(int value_index) { return mods[value_index] == MISSING; }

  // Note that the mod versions take a ModInfo, not an int, as their index.
  static boolean modIsUnmodified(int mod_value) { return mod_value == UNMODIFIED; }
  static boolean modIsModified(int mod_value) { return mod_value == MODIFIED; }
  static boolean modIsMissing(int mod_value) { return mod_value == MISSING; }

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

  /*
   * In output, M=modified, U=unmodified, X=missing.
   * Capital letters indicate the specified modbit does occur,
   * lowercase letters indicate it does not occur.
   */
  static String tuplemodToStringBrief(int tuplemod) {
    return ((tuplemodHasModified(tuplemod) ? "M" : "m")
	    + (tuplemodHasUnmodified(tuplemod) ? "U" : "u")
	    + (tuplemodHasMissing(tuplemod) ? "X" : "x"));
  }


  int tupleMod() {
    boolean[] has_modbit_val = new boolean[MODBIT_VALUES];
    // Extraneous, as the array is initialized to all zeroes.
    for (int i=0; i<MODBIT_VALUES; i++)
      has_modbit_val[i] = false;
    for (int i=0; i<vals.length; i++) {
      has_modbit_val[mods[i]] = true;
    }
    int result = 0;
    // Avoid doing bitwise arithmetic by doing mulitplications and adds.
    for (int i=MODBIT_VALUES-1; i>=0; i--) {
      result *= 2;
      if (has_modbit_val[i])
	result++;
    }
    return result;
  }

  static int parseModified(String raw) {
    int result = new Integer(raw).intValue();
    Assert.assert((result == 0) || (result == 1) || (result == 2));
    return result;
  }

  // The VarInfo form is preferred
  Object getValue(VarInfo vi) { return vi.getValue(this); }
  Object getValue(int val_index) { return vals[val_index]; }


  /** Default constructor that interns its argument. */
  ValueTuple(Object[] vals_, int[] mods_) {
    vals = ArraysMDE.intern(vals_);
    mods = ArraysMDE.intern(mods_);
  }

  // Private constructor that doesn't perform interning.
  private ValueTuple(Object[] vals_, int[] mods_, boolean check) {
    Assert.assert((!check) || (vals_ == ArraysMDE.intern(vals_)));
    Assert.assert((!check) || (mods_ == ArraysMDE.intern(mods_)));
    vals = vals_;
    mods = mods_;
  }

  // More convenient name for the constructor that doesn't intern.
  // This is not private because read_data_trace_file needs it.
  // (The alternative would be for derived variables to take separate
  // vals and mods arguments.)  No one else should use it!
  public static ValueTuple makeUninterned(Object[] vals_, int[] mods_) {
    return new ValueTuple(vals_, mods_, false);
  }


  // Do I need/want this?  Probably in some circumstances...  But in
  // general, users should use the constructor.
  /** Constructor that takes already-interned arguments. */
  static ValueTuple makeFromInterned(Object[] vals_, int[] mods_) {
    return new ValueTuple(vals_, mods_, true);
  }


  // Like clone(), but avoids its problems of default access and returning
  // an Object.
  public ValueTuple shallowcopy() {
    return ValueTuple.makeFromInterned(vals, mods);
  }

  // public Object clone() {
  //   return ValueTuple.makeFromInterned(vals, mods);
  // }


  public int size() {
    Assert.assert(vals.length == mods.length);
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
  // doesn't depend on the elements.
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
      if (vm == ValueAndModified.MISSING)
        deriv.getVarInfo().canBeMissing = true;
    }
    vals = ArraysMDE.intern(new_vals);
    mods = ArraysMDE.intern(new_mods);
  }

  // For debugging
  public String toString() {
    StringBuffer sb = new StringBuffer("[");
    Assert.assert(vals.length == mods.length);
    for (int i=0; i<vals.length; i++) {
      if (i>0)
	sb.append("; ");
      sb.append(vals[i]);
      sb.append(",");
      sb.append(mods[i]);
    }
    sb.append("]");
    return sb.toString();
  }

}
