package utilMDE;

import java.io.Serializable;
import java.util.*;


/**
 * LimitedSizeIntSet stores up to some maximum number of unique non-zero
 * integer values, at which point its rep is nulled, in order to save space.
 **/
public class LimitedSizeIntSet
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20031021L;

  // public final int max_values;

  // If null, then at least num_values distinct values have been seen.
  // The size is not separately stored, because that would take extra space.
  protected int[] values;
  // The number of active elements (equivalently, the first unused index).
  int num_values;

  public LimitedSizeIntSet(int max_values) {
    Assert.assertTrue(max_values > 0);
    // this.max_values = max_values;
    values = new int[max_values];
    num_values = 0;
  }

  public void add(int elt) {
    if (values == null)
      return;

    for (int i=0; i < num_values; i++) {
      if (values[i] == elt) {
        return;
      }
    }
    if (num_values == values.length) {
      values = null;
      num_values++;
      return;
    }
    values[num_values] = elt;
    num_values++;
  }

  public void addAll(LimitedSizeIntSet s) {
    if (repNulled())
      return;
    for (int i=0; i<s.size(); i++) {
      add(s.values[i]);
      if (repNulled()) {
        return;
      }
    }
  }

  public boolean contains(int elt) {
    if (values == null) {
      throw new UnsupportedOperationException();
    }
    for (int i=0; i < num_values; i++) {
      if (values[i] == elt) {
        return true;
      }
    }
    return false;
  }


  /** A lower bound on the number of elements in the set. */
  public int size() {
    return num_values;
  }

  /**
   * An upper bound how many distinct elements can be individually
   * represented in the set.
   **/
  public int max_size() {
    if (values == null) {
      return num_values;
    } else {
      return values.length + 1;
    }
  }

  public boolean repNulled() {
    return values == null;
  }

  public Object clone() {
    LimitedSizeIntSet result;
    try {
      result = (LimitedSizeIntSet) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
    result.values = (int[]) values.clone();
    return result;
  }


  /**
   * Merges a list of LimitedSizeIntSet objects into a single object that
   * represents the values seen by the entire list.  Returns the new
   * object, whose max_values is the same as the first element in the list.
   */
  public static LimitedSizeIntSet merge (int max_values, List /*LimitedSizeIntSet*/ slist) {
    LimitedSizeIntSet result = new LimitedSizeIntSet(max_values);
    for (Iterator itor = slist.iterator(); itor.hasNext(); ) {
      LimitedSizeIntSet s = (LimitedSizeIntSet) itor.next();
      result.addAll(s);
    }
    return result;
  }

  public String toString() {
    return ("[size=" + size() + "; " +
            ((values == null) ? "null" : ArraysMDE.toString(values))
            + "]");
  }

}
