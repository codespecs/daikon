package daikon.inv;

import java.io.Serializable;
import java.util.*;

import utilMDE.*;

import daikon.Global;

// There are three different varieties of add method:
//   add takes one, two, or three Objects; is defined in ValueTracker[123],
//     and calls add_val
//   add_val takes Objects, casts them to their actual types, hashes them
//     to an integer, and calls add_int on them; is defined abstract in
//     ValueTracker[123] and is defined in subtypes.
//   add_int inserts an int


// ValueTracker really ought to be reimplemented in terms of
// LimitedSizeIntSet.


/**
 * ValueTracker stores up to some maximum number of unique non-zero integer
 * values, at which point its rep is nulled.  This is used for efficient
 * justification tests.
 **/
public class ValueTracker
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;
  public final int max_values;

  // These fields are protected so that subclasses can view them, for debugging.

  protected int[] values_cache;

  // Keep track of where the ends of the caches are.  These are the number
  // of active elements (equivalently, the first unused index).
  private int values_end = 0;

  public ValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.values_cache = new int[max_values];
  }

  public int num_values() {
    return values_end;
  }

  private int hashLong(long l) {
    return UtilMDE.hash(l);
  }


  // Here are the versions that do the real work
  protected void add_int(int v1) {
    if (values_cache == null) return;

    for (int i = 0; i < values_end; i++) {
      int elt = values_cache[i];
      if (elt == v1) {
        return;
      }
    }

    values_cache[values_end++] = v1;

    if (values_end == max_values)
      values_cache = null;
  }

  public Object clone() {
    ValueTracker result;
    try {
      result = (ValueTracker) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
    if (values_cache != null) {
      result.values_cache = (int[]) values_cache.clone();
    }
    return result;
  }


  /**
   * Merges a list of ValueTracker objects into a single object that
   * represents the values seen by the entire list.  Returns the new
   * object.
   */
  public static ValueTracker merge (List /*ValueTracker*/ vtlist) {

    // Start with a clone of the first valuetracker in the list
    ValueTracker result
                    = (ValueTracker) ((ValueTracker) vtlist.get (0)).clone();

    // Loop through the remaining value trackers
    for (int i = 1; i < vtlist.size(); i++) {

      // Get the next value tracker
      ValueTracker vt = (ValueTracker) vtlist.get (i);

      // Merge these values into the values_cache
      if (vt.values_cache == null) {
        result.values_cache = null;
        result.values_end = vt.values_end;
      } else {
        for (int j = 0; j < vt.num_values(); j++)
          result.add_int (vt.values_cache[j]);
      }

    }
    return (result);
  }

  public String toString() {
    return ("[num_values=" + num_values() + "]");
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker1
  ///

  public static abstract class ValueTracker1 extends ValueTracker {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTracker1(int max_values) {
      super(max_values);
    }

    /** track the specified object **/
    protected abstract void add_val(Object v1);

    /**
     * Track the specified object, permuting the order of the
     * arguments as necessary.  Obviously unnecessary for this case, but
     * kept for consistency with the two and three variable cases
     */
    public void add(Object v1) {
      add_val(v1);
    }

    public void permute(int[] permutation) {
      Assert.assertTrue(permutation.length == 1);
      Assert.assertTrue(permutation[0] == 0);
    }
  }

  public static class ValueTrackerScalar extends ValueTracker1 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_int(UtilMDE.hash(((Long) v1).longValue()));
    }
  }

  public static class ValueTrackerFloat extends ValueTracker1 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_int(UtilMDE.hash(((Double) v1).doubleValue()));
    }
  }

  public static abstract class ValueTrackerOneArray extends ValueTracker1 {
    // number of arrays with length > 1, for the NoDuplicates invariant
    protected int no_dup_elt_count = 0;

    // SeqIndexComparison only needs to see 8 values before becoming justified
    public final static int max_seq_index_values = 8;
    // Only SeqIndexComparison uses this
    protected int[] seq_index_cache;
    // The number of active elements (equivalently, the first unused index)
    private int seq_index_values_end = 0;

    public ValueTrackerOneArray(int max_values) {
      super(max_values);
      this.seq_index_cache = new int[max_seq_index_values];
    }

    public int num_no_dup_values() {
      return no_dup_elt_count;
    }
    public int num_seq_index_values() {
      return seq_index_values_end;
    }

    public void seq_index_add(int v1) {
      if (seq_index_cache == null) return;
      for (int i = 0; i < max_seq_index_values; i++) {
        int elt = seq_index_cache[i];
        if (elt == v1) {
          return;
        }
      }
      seq_index_cache[seq_index_values_end++] = v1;
      if (seq_index_values_end == max_seq_index_values)
        seq_index_cache = null;
    }

    public Object clone() {
      ValueTrackerOneArray result = (ValueTrackerOneArray) super.clone();
      Assert.assertTrue(result.no_dup_elt_count == this.no_dup_elt_count);
      if (seq_index_cache != null) {
        result.seq_index_cache = (int[]) seq_index_cache.clone();
      }
      return result;
    }

    public static ValueTrackerOneArray merge_vt1a (List /*ValueTrackerOneArray*/ vtlist) {

      ValueTrackerOneArray result = (ValueTrackerOneArray) ValueTracker.merge(vtlist);

      // Loop through the remaining value trackers
      for (int i = 1; i < vtlist.size(); i++) {

        // Get the next value tracker
        ValueTrackerOneArray vt = (ValueTrackerOneArray) vtlist.get (i);

        // Merge these values into the no_dup_elt_count
        result.no_dup_elt_count += vt.no_dup_elt_count;

        // Merge these values into the sequence index cache
        if (vt.seq_index_cache == null) {
          result.seq_index_cache = null;
          result.seq_index_values_end = vt.seq_index_values_end;
        } else {
          for (int j = 0; j < vt.num_seq_index_values(); j++)
            result.seq_index_add (vt.seq_index_cache[j]);
        }
      }
      return (result);
    }

  }

  public static class ValueTrackerScalarArray extends ValueTrackerOneArray {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerScalarArray(int max_values) {
      super(max_values);
    }

    protected void add_val(Object v1) {
      long[] a = (long[]) v1;
      if (a.length > 1)
        this.no_dup_elt_count++;
      add_int(UtilMDE.hash(a));
      // For SeqIndexComparison
      if (seq_index_cache == null) return;
      for (int i = 0; i < a.length; i++) {
        seq_index_add(UtilMDE.hash(a[i], i));
      }
    }

  }

  public static class ValueTrackerFloatArray extends ValueTrackerOneArray {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    // number of arrays with length > 1, for the NoDuplicates invariant
    protected int no_dup_elt_count = 0;

    public ValueTrackerFloatArray(int max_values) {
      super(max_values);
    }

    protected void add_val(Object v1) {
      double[] a = (double[]) v1;
      if (a.length > 1)
        this.no_dup_elt_count++;
      add_int(UtilMDE.hash(a));
      // For SeqIndexComparison
      if (seq_index_cache == null) return;
      for (int i = 0; i < a.length; i++) {
        seq_index_add(UtilMDE.hash(a[i], i));
      }
    }

  }

  public static class ValueTrackerString extends ValueTracker1 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerString(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_int(UtilMDE.hash((String) v1));
    }
  }

  public static class ValueTrackerStringArray extends ValueTracker1 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerStringArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_int(UtilMDE.hash(((String[]) v1)));
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker2
  ///

  public static abstract class ValueTracker2 extends ValueTracker {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    private boolean swap = false;
    public ValueTracker2(int max_values) {
      super(max_values);
    }

    /** track the specified objects **/
    protected abstract void add_val(Object v1, Object v2);

    /**
     * Track the specified object, permuting the order of the
     * arguments as necessary.
     */
    public void add(Object v1, Object v2) {
      if ((this instanceof ValueTrackerFloatArrayFloat)
          || (this instanceof ValueTrackerScalarArrayScalar))
        add_val (v1, v2);
      else {
        if (swap)
          add_val (v2, v1);
        else
          add_val (v1, v2);
      }
    }
    public void permute(int[] permutation) {
      Assert.assertTrue(permutation.length == 2);
      if (permutation[0] == 1)
        swap = !swap;
    }

  }

  public static class ValueTrackerTwoString extends ValueTracker2 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoString(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_int(UtilMDE.hash((String) v1, (String) v2));
    }
  }

  public static class ValueTrackerTwoScalar extends ValueTracker2 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_int(UtilMDE.hash(((Long) v1).longValue(), ((Long) v2).longValue()));
    }
  }

  public static class ValueTrackerTwoScalarDebugging extends ValueTrackerTwoScalar {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoScalarDebugging(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      long l1 = ((Long) v1).longValue();
      long l2 = ((Long) v2).longValue();
      // System.err.println("ValueTrackerTwoScalar: " + l1 + ", " + l2
      //                    + " <= " + toString() + "\n  "
      //                    + ArraysMDE.toString(this.values_cache));
      super.add_val(v1, v2);
      // System.err.println("ValueTrackerTwoScalar: " + l1 + ", " + l2
      //                    + " => " + toString() + "\n  "
      //                    + ArraysMDE.toString(this.values_cache));
    }
  }

  public static class ValueTrackerTwoFloat extends ValueTracker2 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_int(UtilMDE.hash(((Double) v1).doubleValue(),
                            ((Double) v2).doubleValue()));
    }
  }

  // SeqIntComparison Invariants are the only ones so far that use this, so
  // I matched this method up with SeqIntComparison's old notions of
  // "values" (if a sample is a[] and b, then each pair {a[i],b} is a
  // value).
  public static class ValueTrackerScalarArrayScalar extends ValueTracker2 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerScalarArrayScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      long[] a = (long[]) v1;
      long d = ((Long) v2).longValue();
      for (int i = 0; i < a.length; i++) {
        add_int(UtilMDE.hash(a[i], d));
      }
    }
  }

  public static class ValueTrackerFloatArrayFloat extends ValueTracker2 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerFloatArrayFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      double[] a = (double[]) v1;
      double d = ((Double) v2).doubleValue();
      for (int i = 0; i < a.length; i++) {
        add_int(UtilMDE.hash(a[i], d));
      }
    }
  }

  public static abstract class ValueTrackerTwoArray extends ValueTracker2 {

    // PairwiseIntComparison only needs to see 8 values before becoming justified
    public final static int max_elt_values = 8;
    // Only PairwiseIntComparison uses this
    protected int[] elt_values_cache;
    // The number of active elements (equivalently, the first unused index).
    private int elt_values_end = 0;

    public ValueTrackerTwoArray(int max_values) {
      super(max_values);
      this.elt_values_cache = new int[max_elt_values];
    }

    public int num_elt_values() {
      return elt_values_end;
    }

    public void elt_add(int v1) {
      if (elt_values_cache == null) return;

      for (int i = 0; i < max_elt_values; i++) {
        int elt = elt_values_cache[i];
        if (elt == v1) {
          return;
        }
      }
      elt_values_cache[elt_values_end++] = v1;

      if (elt_values_end == max_elt_values)
        elt_values_cache = null;
    }

    public Object clone() {
      ValueTrackerTwoArray result = (ValueTrackerTwoArray) super.clone();
      if (elt_values_cache != null) {
        result.elt_values_cache = (int[]) elt_values_cache.clone();
      }
      return result;
    }

    public static ValueTrackerTwoArray merge_vt2a (List /*ValueTrackerTwoArray*/ vtlist) {

      ValueTrackerTwoArray result = (ValueTrackerTwoArray) ValueTracker.merge(vtlist);

      // Loop through the remaining value trackers
      for (int i = 1; i < vtlist.size(); i++) {

        // Get the next value tracker
        ValueTrackerTwoArray vt = (ValueTrackerTwoArray) vtlist.get (i);

        // Merge these values into the elt_values_cache
        if (vt.elt_values_cache == null) {
          result.elt_values_cache = null;
          result.elt_values_end = vt.elt_values_end;
        } else {
          for (int j = 0; j < vt.num_elt_values(); j++)
            result.elt_add (vt.elt_values_cache[j]);
        }
      }
      return (result);
    }

  }

  public static class ValueTrackerTwoScalarArray extends ValueTrackerTwoArray {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoScalarArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      long[] a1 = (long[]) v1;
      long[] a2 = (long[]) v2;
      add_int(UtilMDE.hash(a1, a2));

      if (elt_values_cache == null) return;
      if (a1.length != a2.length) return;
      for (int i=0; i < a1.length; i++) {
        elt_add(UtilMDE.hash(a1[i], a2[i]));
      }
    }
  }

  public static class ValueTrackerTwoFloatArray extends ValueTrackerTwoArray {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerTwoFloatArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      double[] a1 = (double[]) v1;
      double[] a2 = (double[]) v2;
      add_int(UtilMDE.hash(a1, a2));

      if (elt_values_cache == null) return;
      if (a1.length != a2.length) return;
      for (int i=0; i < a1.length; i++) {
        elt_add(UtilMDE.hash(a1[i], a2[i]));
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker3
  ///

  public static abstract class ValueTracker3 extends ValueTracker {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    final static int order_123 = 0;
    final static int order_213 = 1;
    final static int order_312 = 2;
    final static int order_132 = 3;
    final static int order_231 = 4;
    final static int order_321 = 5;
    final static int[][] var_indices;
    static {
      var_indices = new int[6][];
      var_indices[order_123] = new int[] { 0, 1, 2 };
      var_indices[order_213] = new int[] { 1, 0, 2 };
      var_indices[order_312] = new int[] { 2, 0, 1 };
      var_indices[order_132] = new int[] { 0, 2, 1 };
      var_indices[order_231] = new int[] { 1, 2, 0 };
      var_indices[order_321] = new int[] { 2, 1, 0 };
    }
    private int order = order_123;

    public ValueTracker3(int max_values) {
      super(max_values);
    }

    /** track the specified objects **/
    protected abstract void add_val(Object v1, Object v2, Object v3);

    /**
     * Track the specified object, permuting the order of the
     * arguments as necessary.
     */
    public void add(Object v1, Object v2, Object v3) {

      switch (order) {
        case order_123: add_val (v1, v2, v3); break;
        case order_213: add_val (v2, v1, v3); break;
        case order_312: add_val (v3, v1, v2); break;
        case order_132: add_val (v1, v3, v2); break;
        case order_231: add_val (v2, v3, v1); break;
        case order_321: add_val (v3, v2, v1); break;
      }
    }

    /** permutation is from the old position to the new position */
    public void permute(int[] permutation) {
      Assert.assertTrue(permutation.length == 3);
      int[] new_order = new int[3];
      int[] old_order = var_indices[order];
      new_order[0] = old_order[permutation[0]];
      new_order[1] = old_order[permutation[1]];
      new_order[2] = old_order[permutation[2]];
      for (int i = 0; i < var_indices.length; i++) {
        if (Arrays.equals (new_order, var_indices[i])) {
          order = i;
          return;
        }
      }
    Assert.assertTrue(false, "Could not find new ordering");
    }
  }

  public static class ValueTrackerThreeScalar extends ValueTracker3 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerThreeScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2, Object v3) {
      add_int(UtilMDE.hash(((Long) v1).longValue(),
                           ((Long) v2).longValue(),
                           ((Long) v3).longValue()));
    }
  }

  public static class ValueTrackerThreeFloat extends ValueTracker3 {
    // We are Serializable, so we specify a version to allow changes to
    // method signatures without breaking serialization.  If you add or
    // remove fields, you should change this number to the current date.
    static final long serialVersionUID = 20031017L;

    public ValueTrackerThreeFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2, Object v3) {
      add_int(UtilMDE.hash(((Double) v1).longValue(),
                           ((Double) v2).longValue(),
                           ((Double) v3).longValue()));
    }
  }

}
