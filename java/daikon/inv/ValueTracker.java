package daikon.inv;

import java.io.Serializable;

import utilMDE.*;

import daikon.Global;
import daikon.inv.ternary.threeScalar.LinearTernaryCore;
import daikon.inv.ternary.threeScalar.LinearTernaryCoreFloat;

/**
 * ValueTracker stores up to some maximum number of unique non-zero integer
 * values, at which point its rep is nulled.  This is used for efficienct
 * justification tests.
 *
 * Declared final only for efficiency.
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

  private double[] values_cache;

  private double[] seq_index_cache;

  private double[] elt_values_cache;

  // Keep track of MINTRIPLES points seen to be used by computeProbability
  // in LinearTernary
  private double[][] point_tracker = new double[LinearTernaryCore.minTriples()][3];

  // Keep track of where the ends of the caches are
  private int values_end = 0;
  private int elt_values_end = 0;
  private int seq_index_values_end = 0;
  private int point_tracker_end = 0;

  private int no_dup_elt_count; // Keeps track of number of arrays with length > 1
                                   // for the NoDuplicates invariant

  public ValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.no_dup_elt_count = 0;
    this.values_cache = new double[max_values];
    this.elt_values_cache = new double[max_elt_values]; // Only PairwiseIntComparison uses this
    this.seq_index_cache = new double[max_seq_index_values]; // Only SeqIndexComparison uses this
  }

  public int num_no_dup_values() {
    return no_dup_elt_count;
  }

  public int num_values() {
    return values_end;
  }

  public int num_point_values() {
    return point_tracker_end;
  }

  public int num_seq_index_values() {
    return seq_index_values_end;
  }

  public int num_elt_values() {
    return elt_values_end;
  }

  private int hashLong(long l) {
    return (int) (l ^ (l >> 32));
  }

  public void add(String v1, String v2) {
    if (values_cache == null) return;
    if ((v1 == null) || (v2 == null)) return;
    add(v1.hashCode(), v2.hashCode());
  }

  public void add(String v1) {
    if (values_cache == null) return;
    if (v1 == null) return;
    add(v1.hashCode());
  }

  public void add(String[] v1) {
    if (values_cache == null) return;
    if (v1 == null) return;
    long av1 = 0;
    for (int i = 0; i < v1.length; i++) {
      if (v1[i] == null) continue;
      av1 ^= v1[i].hashCode();
    }
    add(av1);
  }

  public void add(long[] v1, long[] v2) {
    // The values_cache will always reach its capacity before
    // the elt_values_cache, by definition
    if (values_cache != null) {
      long av1 = 0;
      for (int i = 0; i < v1.length; i++) {
        av1 ^= v1[i];
      }
      long av2 = 0;
      for (int i = 0; i < v2.length; i++) {
        av2 ^= v2[i];
      }
      add(av1, av2);
    }

    if (elt_values_cache == null) return;
    if (v1.length != v2.length) return;
    for (int i=0; i < v1.length; i++) {
      long temp = (v1[i] ^ v2[i]) + 10;
      elt_add((int)(temp ^ (temp >> 32)));
    }
  }

  public void add(long[] v1) {
    if (v1.length > 1)
      no_dup_elt_count++;
    if (values_cache != null) {
      for (int i = 0; i < v1.length; i++) {
        // long av1 = 0;
        // Only SeqIndexComparison uses this, so let's use its notion
        // of value (each distinct pair (a[i], i))
        long temp = ((37 * v1[i]) + i) + 10;
        add(hashLong(temp));
        // add(v1[i] ^ i); // av1 ^= v1[i];
      }
    // add(av1);
    }

    if (seq_index_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      long temp = ((37 * v1[i]) + i) + 10;
      seq_index_add(hashLong(temp));
    }
  }

  // SeqIntComparison Invariants are the only ones so far that use this,
  // so I matched this method up with SeqIntComparison's old notions of "values"
  // (if a sample is a[] and b, then each pair {a[i],b} is a value)
  public void add(long[] v1, long v2) {
    if (values_cache == null) return;
    long av1 = 0;
    for (int i = 0; i < v1.length; i++) {
      add(v1[i], v2);
    }
  }

  public void add(long v1, long v2, long v3) {
    if (values_cache != null) {
      add((37*((37*hashLong(v1)) + hashLong(v2))) + hashLong(v3));
    }

    if (point_tracker == null) return;
    // Add (v1,v2,v3) to the point_tracker if it hasn't been seen before
    for (int i=0; i < point_tracker_end; i++) {
      if ((v1 == point_tracker[i][0]) && (v2 == point_tracker[i][1]) &&
          (v3 == point_tracker[i][2]))
        return;
    }
    point_tracker[point_tracker_end][0] = v1;
    point_tracker[point_tracker_end][1] = v2;
    point_tracker[point_tracker_end][2] = v3;
    if (++point_tracker_end == LinearTernaryCore.minTriples())
      point_tracker = null;
  }

  public void add(long v1, long v2) {
    if (values_cache == null) return;
    add((37*hashLong(v1)) + hashLong(v2));
  }

  public void add(long v1) {
    if (values_cache == null) return;
    // if ((v1 == 0) || (v1 == -1)) return;
    if (!Global.old_tracker)
      v1 = v1 + 10; // Do this so that -1 and 0 don't both hash to the same value
    add(hashLong(v1));
  }

  public void add(int v1, int v2) {
    if (values_cache == null) return;
    add(v1 ^ v2);
  }

  public void add(int v1) {
    if (values_cache == null) return;

    for (int i = 0; i < ((Global.old_tracker) ? max_values : values_end); i++) {
      double elt = values_cache[i];
      if (Global.old_tracker) {
        if (elt == 0) {
          values_cache[i] = v1;
          return;
        }
      }
      if (elt == v1) {
        return;
      }
    }

    if (!Global.old_tracker) {
      values_cache[values_end++] = v1;

      if (values_end == max_values)
        values_cache = null;
    } else {
      values_cache = null;
    }
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

  public void add(double[] v1, double[] v2) {
    if (values_cache != null) {
      double av1 = 0;
      for (int i = 0; i < v1.length; i++) {
        av1 = av1 * 5 + v1[i];
      }
      double av2 = 0;
      for (int i = 0; i < v2.length; i++) {
        av2 = av2 * 7 + v2[i];
      }
      add(av1, av2);
    }

    if (elt_values_cache == null) return;
    if (v1.length != v2.length) return;
    for (int i=0; i < v1.length; i++) {
      elt_add((v1[i] * 23 ) * v2[i]);
    }
  }

  public void add(double[] v1) {
    if (v1.length > 1)
      no_dup_elt_count++;
    if (values_cache != null) {
      // double av1 = 0;
      for (int i = 0; i < v1.length; i++) {
        add((v1[i] * 23) * i); // av1 = av1 * 5 + v1[i]; for SeqIndexComparison
      }
      // add(av1);
    }

    if (seq_index_cache == null) return;
    for (int i = 0; i < v1.length; i++) {
      seq_index_add((v1[i]*23)*i);
    }
  }

  // See the comment above add(long[], long)
  public void add(double[] v1, double v2) {
    if (values_cache == null) return;
    double av1 = 0;
    for (int i = 0; i < v1.length; i++) {
      add(v1[i], v2);
    }
  }

  public void add(double v1, double v2, double v3) {
    if (values_cache != null) {
      add(((v1 * 17) + v2 * 13) + v3);
    }

    if (point_tracker == null) return;
    // Add (v1,v2,v3) to the point_tracker if it hasn't been seen before
    for (int i=0; i < point_tracker_end; i++) {
      if ((v1 == point_tracker[i][0]) && (v2 == point_tracker[i][1]) &&
          (v3 == point_tracker[i][2]))
        return;
    }
    point_tracker[point_tracker_end][0] = v1;
    point_tracker[point_tracker_end][1] = v2;
    point_tracker[point_tracker_end][2] = v3;
    if (++point_tracker_end == LinearTernaryCore.minTriples())
      point_tracker = null;
  }

  public void add(double v1, double v2) {
    if (values_cache == null) return;
    add((v1 * 23) * v2);
  }

  public void add(double v1) {
    if (values_cache == null) return;

    for (int i = 0; i < ((Global.old_tracker) ? max_values : values_end); i++) {
      double elt = values_cache[i];
      if (Global.old_tracker) {
        if (elt == 0) {
          values_cache[i] = v1;
          return;
        }
      }
      if (elt == v1) {
        return;
      }
    }

    if (!Global.old_tracker) {
      values_cache[values_end++] = v1;

      if (values_end == max_values)
        values_cache = null;
    } else {
      values_cache = null;
    }
  }

  public void elt_add(double v1) {
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

  public void seq_index_add(double v1) {
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

  public Object clone() {
    try {
      ValueTracker result = (ValueTracker) super.clone();
      if (values_cache != null) {
        result.values_cache = (double[]) values_cache.clone();
      }
      if (elt_values_cache != null) {
        elt_values_cache = (double[]) elt_values_cache.clone();
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

  public static abstract class ValueTracker1 extends ValueTracker {
    public ValueTracker1(int max_values) {
      super(max_values);
    }

    public abstract void add(Object v1);
  }

  public static abstract class ValueTracker2 extends ValueTracker {
    public ValueTracker2(int max_values) {
      super(max_values);
    }
    public abstract void add(Object v1, Object v2);
  }

  public static abstract class ValueTracker3 extends ValueTracker {
    public ValueTracker3(int max_values) {
      super(max_values);
    }
    public abstract void add(Object v1, Object v2, Object v3);
  }

  public static class ValueTrackerScalar extends ValueTracker1 {
    public ValueTrackerScalar(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(((Long) v1).longValue());
    }
  }

  public static class ValueTrackerFloat extends ValueTracker1 {
    public ValueTrackerFloat(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add(((Double) v1).doubleValue());
    }
  }

  public static class ValueTrackerScalarArray extends ValueTracker1 {
    public ValueTrackerScalarArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add((long[]) v1);
    }
  }

  public static class ValueTrackerFloatArray extends ValueTracker1 {
    public ValueTrackerFloatArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add((double[]) v1);
    }
  }

  public static class ValueTrackerString extends ValueTracker1 {
    public ValueTrackerString(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add((String) v1);
    }
  }

  public static class ValueTrackerStringArray extends ValueTracker1 {
    public ValueTrackerStringArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1) {
      add((String[]) v1);
    }
  }

  public static class ValueTrackerTwoString extends ValueTracker2 {
    public ValueTrackerTwoString(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add((String) v1, (String) v2);
    }
  }

  public static class ValueTrackerTwoScalar extends ValueTracker2 {
    public ValueTrackerTwoScalar(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add(((Long) v1).longValue(), ((Long) v2).longValue());
    }
  }

  public static class ValueTrackerTwoFloat extends ValueTracker2 {
    public ValueTrackerTwoFloat(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add(((Double) v1).doubleValue(), ((Double) v2).doubleValue());
    }
  }

  public static class ValueTrackerFloatArrayFloat extends ValueTracker2 {
    public ValueTrackerFloatArrayFloat(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add((double[]) v1, ((Double) v2).doubleValue());
    }
  }

  public static class ValueTrackerScalarArrayScalar extends ValueTracker2 {
    public ValueTrackerScalarArrayScalar(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add((long[]) v1, ((Long) v2).longValue());
    }
  }

  public static class ValueTrackerTwoFloatArray extends ValueTracker2 {
    public ValueTrackerTwoFloatArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add((double[]) v1, ((double[]) v2));
    }
  }

  public static class ValueTrackerTwoScalarArray extends ValueTracker2 {
    public ValueTrackerTwoScalarArray(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2) {
      add((long[]) v1, ((long[]) v2));
    }
  }

  public static class ValueTrackerThreeScalar extends ValueTracker3 {
    public ValueTrackerThreeScalar(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2, Object v3) {
      add(((Long) v1).longValue(), ((Long) v2).longValue(), ((Long) v3).longValue());
    }
  }

  public static class ValueTrackerThreeFloat extends ValueTracker3 {
    public ValueTrackerThreeFloat(int max_values) {
      super(max_values);
    }
    public void add(Object v1, Object v2, Object v3) {
      add(((Double) v1).longValue(), ((Double) v2).longValue(), ((Double) v3).longValue());
    }
  }

}
