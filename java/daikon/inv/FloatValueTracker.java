package daikon.inv;

import java.io.Serializable;

import utilMDE.*;

/**
 * FloatValueTracker stores up to some maximum number of unique non-zero values,
 * at which point its rep is nulled.  This is used for efficienct
 * justification tests.
 *
 * Declared final only for efficiency.
 **/
public final class FloatValueTracker
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public final int max_values;

  private double[] values_cache;

  public FloatValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.values_cache = new double[max_values];
  }

  public int num_values() {
    if (values_cache == null) {
      return max_values;
    }
    int result = ArraysMDE.indexOf(values_cache, 0);
    if (result == -1) {
      result = max_values;
    }
    return result;
  }

  public void add(double[] v1, double[] v2) {
    if (values_cache == null) return;
    add(UtilMDE.hashToDouble(UtilMDE.hashToDouble(v1), UtilMDE.hashToDouble(v2)));
  }

  public void add(double v1, double v2, double v3) {
    if (values_cache == null) return;
    add(UtilMDE.hashToDouble(v1, v2, v3));
  }

  public void add(double v1, double v2) {
    if (values_cache == null) return;
    add(UtilMDE.hashToDouble(v1, v2));
  }

  public void add(double v1) {
    if (values_cache == null) return;

    for (int i = 0; i < max_values; i++) {
      double elt = values_cache[i];
      if (elt == 0) {
        values_cache[i] = v1;
        return;
      }
      if (elt == v1) {
        return;
      }
    }

    values_cache = null;
  }

  public Object clone() {
    try {
      FloatValueTracker result = (FloatValueTracker) super.clone();
      if (values_cache != null) {
        result.values_cache = (double[]) values_cache.clone();
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

}
