package daikon.inv;

import java.io.Serializable;
import java.util.*;

import utilMDE.*;

import daikon.Global;

// There are three different varieties of add method:
//   add takes one, two, or three Objects; is defined in ValueTracker[123],
//     and calls add_val
//   add_val takes Objects, casts them to their actual types, and calls
//     add_prim on them; is defined abstract in ValueTracker[123] and is
//     defined in subtypes.
//   add_prim converts (numeric) values into ints, and inserts the ints


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

  // 8 was chosen because the only Invariants that use this,
  // the PairwiseIntComparisons, only need to see 8 values
  // before their computeProbability() methods return something
  // justified
  public final static int max_elt_values = 8;

  // Also 8 because SeqIndexComparison only needs to see 8
  // values before becoming justified
  public final static int max_seq_index_values = 8;

  // These fields are protected so that subclasses can view them, for debugging.

  protected int[] values_cache;

  // Only SeqIndexComparison uses this
  protected int[] seq_index_cache;

  // Only PairwiseIntComparison uses this
  protected int[] elt_values_cache;

  // Keep track of where the ends of the caches are.  These are the number
  // of active elements (equivalently, the first unused index).
  private int values_end = 0;
  private int elt_values_end = 0;
  private int seq_index_values_end = 0;

  // number of arrays with length > 1, for the NoDuplicates invariant
  private int no_dup_elt_count;

  public ValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.no_dup_elt_count = 0;
    this.values_cache = new int[max_values];
    this.elt_values_cache = new int[max_elt_values];
    this.seq_index_cache = new int[max_seq_index_values];
  }

  public int num_no_dup_values() {
    return no_dup_elt_count;
  }

  public int num_values() {
    return values_end;
  }

  public int num_seq_index_values() {
    return seq_index_values_end;
  }

  public int num_elt_values() {
    return elt_values_end;
  }

  private int hashLong(long l) {
    return UtilMDE.hash(l);
  }

  protected void add_prim(String v1, String v2) {
    if (values_cache == null) return;
    if ((v1 == null) || (v2 == null)) return;
    add_prim (v1.hashCode(), v2.hashCode());
  }

  protected void add_prim(String v1) {
    if (values_cache == null) return;
    if (v1 == null) return;
    add_prim (v1.hashCode());
  }

  protected void add_prim(String[] v1) {
    if (values_cache == null) return;
    if (v1 == null) return;
    long av1 = 0;
    for (int i = 0; i < v1.length; i++) {
      if (v1[i] == null) continue;
      av1 ^= v1[i].hashCode();
    }
    add_prim (av1);
  }

  protected void add_prim(long[] v1, long[] v2) {
    // The values_cache will always reach its capacity before
    // the elt_values_cache (or at the same time), by definition
    if (values_cache != null) {
      add_prim(UtilMDE.hash(v1), UtilMDE.hash(v2));
    }

    if (elt_values_cache == null) return;
    if (v1.length != v2.length) return;
    for (int i=0; i < v1.length; i++) {
      long temp = (v1[i] ^ v2[i]) + 10;
      elt_add((int)(temp ^ (temp >> 32)));
    }
  }

  protected void add_prim(long[] v1) {
    if (v1.length > 1)
      no_dup_elt_count++;
    if (values_cache != null) {
      for (int i = 0; i < v1.length; i++) {
        // Only SeqIndexComparison uses this, so let's use its notion
        // of value (each distinct pair (a[i], i))
        add_prim(UtilMDE.hash(v1[i], i));
      }
    }

    if (seq_index_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      seq_index_add(UtilMDE.hash(v1[i], i));
    }
  }

  // SeqIntComparison Invariants are the only ones so far that use this,
  // so I matched this method up with SeqIntComparison's old notions of "values"
  // (if a sample is a[] and b, then each pair {a[i],b} is a value)
  protected void add_prim(long[] v1, long v2) {
    if (values_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      add_prim(v1[i], v2);
    }
  }

  protected void add_prim(long v1, long v2, long v3) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hash(v1, v2, v3));
  }

  protected void add_prim(long v1, long v2) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hash(v1, v2));
  }

  protected void add_prim(long v1) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hash(v1));
  }

  protected void add_prim(int v1, int v2) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hash(v1, v2));
  }

  protected void add_prim(int v1) {
    if (values_cache == null) return;

    for (int i = 0; i < values_end; i++) {
      double elt = values_cache[i];
      if (elt == v1) {
        return;
      }
    }

    values_cache[values_end++] = v1;

    if (values_end == max_values)
      values_cache = null;
  }

  public void elt_add(int v1) {
    if (elt_values_cache == null) return;

    for (int i = 0; i < max_elt_values; i++) {
      double elt = elt_values_cache[i];
      if (elt == v1) {
        return;
      }
    }
    elt_values_cache[elt_values_end++] = v1;

    if (elt_values_end == max_elt_values)
      elt_values_cache = null;
  }

  public void seq_index_add(int v1) {
    if (seq_index_cache == null) return;

    for (int i = 0; i < max_seq_index_values; i++) {
      double elt = seq_index_cache[i];
      if (elt == v1) {
        return;
      }
    }
    seq_index_cache[seq_index_values_end++] = v1;

    if (seq_index_values_end == max_seq_index_values)
      seq_index_cache = null;
  }

  protected void add_prim(double[] v1, double[] v2) {
    if (values_cache != null) {
      add_prim(UtilMDE.hashToDouble(v1), UtilMDE.hashToDouble(v2));
    }

    if (elt_values_cache == null) return;
    if (v1.length != v2.length) return;
    for (int i=0; i < v1.length; i++) {
      elt_add(UtilMDE.hashToDouble(v1[i], v2[i]));
    }
  }

  protected void add_prim(double[] v1) {
    if (v1.length > 1)
      no_dup_elt_count++;
    if (values_cache != null) {
      for (int i = 0; i < v1.length; i++) {
        add_prim(UtilMDE.hashToDouble(v1[i], i));
      }
    }

    if (seq_index_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      seq_index_add(UtilMDE.hashToDouble(v1[i], i));
    }
  }

  // See the comment above add_prim(long[], long)
  protected void add_prim(double[] v1, double v2) {
    if (values_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      add_prim(v1[i], v2);
    }
  }

  protected void add_prim(double v1, double v2, double v3) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hashToDouble(v1, v2, v3));
  }

  protected void add_prim(double v1, double v2) {
    if (values_cache == null) return;
    add_prim(UtilMDE.hashToDouble(v1, v2));
  }

  protected void add_prim(double v1) {
    if (values_cache == null) return;

    for (int i = 0; i < values_end; i++) {
      double elt = values_cache[i];
      if (elt == v1) {
        return;
      }
    }

    values_cache[values_end++] = UtilMDE.hash(v1);

    if (values_end == max_values)
      values_cache = null;
  }

  public void elt_add(double v1) {
    if (elt_values_cache == null) return;

    for (int i = 0; i < max_elt_values; i++) {
      double elt = elt_values_cache[i];
      if (elt == v1) {
        return;
      }
    }
    elt_values_cache[elt_values_end++] = UtilMDE.hash(v1);

    if (elt_values_end == max_elt_values)
      elt_values_cache = null;
  }

  public void seq_index_add(double v1) {
    if (seq_index_cache == null) return;

    for (int i = 0; i < max_seq_index_values; i++) {
      double elt = seq_index_cache[i];
      if (elt == v1) {
        return;
      }
    }
    seq_index_cache[seq_index_values_end++] = UtilMDE.hash(v1);

    if (seq_index_values_end == max_seq_index_values)
      seq_index_cache = null;
  }

  public Object clone() {
    try {
      ValueTracker result = (ValueTracker) super.clone();
      if (values_cache != null) {
        result.values_cache = (int[]) values_cache.clone();
      }
      if (elt_values_cache != null) {
        result.elt_values_cache = (int[]) elt_values_cache.clone();
      }
      if (seq_index_cache != null) {
        result.seq_index_cache = (int[]) seq_index_cache.clone();
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
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
          result.add_prim (vt.values_cache[j]);
      }

      // Merge these values into the sequence index cache
      if (vt.seq_index_cache == null) {
        result.seq_index_cache = null;
        result.seq_index_values_end = vt.seq_index_values_end;
      } else {
        for (int j = 0; j < vt.num_seq_index_values(); j++)
          result.seq_index_add (vt.seq_index_cache[j]);
      }

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

  public String toString() {
    return ("[num_values=" + num_values() + ", num_seq_index_values=" +
            num_seq_index_values() + ", num_elt_values=" + num_elt_values()
            + "]");
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker1
  ///

  public static abstract class ValueTracker1 extends ValueTracker {
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
    public ValueTrackerScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_prim(((Long) v1).longValue());
    }
  }

  public static class ValueTrackerFloat extends ValueTracker1 {
    public ValueTrackerFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_prim(((Double) v1).doubleValue());
    }
  }

  public static class ValueTrackerScalarArray extends ValueTracker1 {
    public ValueTrackerScalarArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
    	add_prim((long[]) v1);
    }
  }

  public static class ValueTrackerFloatArray extends ValueTracker1 {
    public ValueTrackerFloatArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_prim((double[]) v1);
    }
  }

  public static class ValueTrackerString extends ValueTracker1 {
    public ValueTrackerString(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_prim((String) v1);
    }
  }

  public static class ValueTrackerStringArray extends ValueTracker1 {
    public ValueTrackerStringArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1) {
      add_prim((String[]) v1);
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker2
  ///

  public static abstract class ValueTracker2 extends ValueTracker {
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
    public ValueTrackerTwoString(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim((String) v1, (String) v2);
    }
  }

  public static class ValueTrackerTwoScalar extends ValueTracker2 {
    public ValueTrackerTwoScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim(((Long) v1).longValue(), ((Long) v2).longValue());
    }
  }

  public static class ValueTrackerTwoScalarDebugging extends ValueTrackerTwoScalar {
    public ValueTrackerTwoScalarDebugging(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      long l1 = ((Long) v1).longValue();
      long l2 = ((Long) v2).longValue();
      System.err.println("ValueTrackerTwoScalar: " + l1 + ", " + l2
                         + " <= " + toString() + "\n  "
                         + ArraysMDE.toString(this.values_cache));
      super.add_val(v1, v2);
      System.err.println("ValueTrackerTwoScalar: " + l1 + ", " + l2
                         + " => " + toString() + "\n  "
                         + ArraysMDE.toString(this.values_cache));
    }
  }

  public static class ValueTrackerTwoFloat extends ValueTracker2 {
    public ValueTrackerTwoFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim(((Double) v1).doubleValue(), ((Double) v2).doubleValue());
    }
  }

  public static class ValueTrackerFloatArrayFloat extends ValueTracker2 {
    public ValueTrackerFloatArrayFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim((double[]) v1, ((Double) v2).doubleValue());
    }
  }

  public static class ValueTrackerScalarArrayScalar extends ValueTracker2 {
    public ValueTrackerScalarArrayScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim((long[]) v1, ((Long) v2).longValue());
    }
  }

  public static class ValueTrackerTwoFloatArray extends ValueTracker2 {
    public ValueTrackerTwoFloatArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim((double[]) v1, ((double[]) v2));
    }
  }

  public static class ValueTrackerTwoScalarArray extends ValueTracker2 {
    public ValueTrackerTwoScalarArray(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2) {
      add_prim((long[]) v1, ((long[]) v2));
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// ValueTracker3
  ///

  public static abstract class ValueTracker3 extends ValueTracker {
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
    public ValueTrackerThreeScalar(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2, Object v3) {
      add_prim(((Long) v1).longValue(), ((Long) v2).longValue(), ((Long) v3).longValue());
    }
  }

  public static class ValueTrackerThreeFloat extends ValueTracker3 {
    public ValueTrackerThreeFloat(int max_values) {
      super(max_values);
    }
    protected void add_val(Object v1, Object v2, Object v3) {
      add_prim(((Double) v1).longValue(), ((Double) v2).longValue(), ((Double) v3).longValue());
    }
  }

}
