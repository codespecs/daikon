package daikon.inv;

import java.io.Serializable;

import utilMDE.*;

/**
 * StringValueTracker stores up to some maximum number of unique non-null values,
 * at which point its rep is nulled.  This is used for efficienct
 * justification tests.
 *
 * Declared final only for efficiency.
 **/
public final class StringValueTracker
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  public final int max_values;

  private String[] values_cache;

  public StringValueTracker(int max_values) {
    Assert.assertTrue(max_values > 0);
    this.max_values = max_values;
    this.values_cache = new String[max_values];
  }

  public int num_values() {
    if (values_cache == null) {
      return max_values;
    }
    int result = ArraysMDE.indexOf(values_cache, null);
    if (result == -1) {
      result = max_values;
    }
    return result;
  }

  public void add(String[] v1, String[] v2) {
    if (values_cache == null) return;
    add(ArraysMDE.toString(v1, false), ArraysMDE.toString(v2, false));
  }

  public void add(String v1, String v2, String v3) {
    if (values_cache == null) return;
    add (v1 + v2 + v3);
  }

  public void add(String v1, String v2) {
    if (values_cache == null) return;
    add(v1 + v2);
  }

  public void add(String v1) {
    if (values_cache == null) return;

    for (int i = 0; i < max_values; i++) {
      String elt = values_cache[i];
      if (elt == null) {
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
      StringValueTracker result = (StringValueTracker) super.clone();
      if (values_cache != null) {
        result.values_cache = (String[]) values_cache.clone();
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

}
