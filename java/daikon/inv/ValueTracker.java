package daikon.inv;

import java.io.Serializable;

import utilMDE.*;

/**
 * ValueTracker stores up to some maximum number of unique values, at
 * which point it rep is nulled.  This is used for efficienct
 * justification tests.
 *
 * Declared final only for efficiency.
 **/
public final class ValueTracker
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public final int max_values;

  private int[] values_cache;

  public ValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.values_cache = new int[max_values];
  }

  public int num_values() {
    if (values_cache != null) {
      int result = ArraysMDE.indexOf(values_cache, 0);
      if (result == -1) {
        result = max_values;
      }
      return result;
    } else {
      return max_values;
    }
  }

  public void add(String v1, String v2) {
    if (values_cache == null) return;
    add(v1.hashCode(), v2.hashCode());
  }

  public void add(String v1) {
    if (values_cache == null) return;
    add(v1.hashCode());
  }

  public void add(long[] v1, long[] v2) {
    if (values_cache == null) return;
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

  public void add(long v1, long v2, long v3) {
    if (values_cache == null) return;
    add(v1 ^ v2 ^ v3);
  }

  public void add(long v1, long v2) {
    if (values_cache == null) return;
    add(v1 ^ v2);
  }

  public void add(long v1) {
    if (values_cache == null) return;
    add((int)(v1 ^ (v1 >> 32)));
  }

  public void add(int v1, int v2) {
    if (values_cache == null) return;
    add(v1 ^ v2);
  }

  public void add(int v1) {
    if (values_cache == null) return;

    for (int i = 0; i < max_values; i++) {
      int elt = values_cache[i];
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
      ValueTracker result = (ValueTracker) super.clone();
      if (values_cache != null) {
        result.values_cache = (int[]) values_cache.clone();
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

}
