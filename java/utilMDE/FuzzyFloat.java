package utilMDE;

/**
 * Routines for doing 'fuzzy' floating point comparisons.  Those are
 * comparisons that only require the floating point numbers to be
 * relatively close to one another to be equal, rather than exactly
 * equal
 */

import java.util.*;

public class FuzzyFloat {

  double min_ratio = 0.9999;
  double max_ratio = 1.0001;
  boolean off = false;

  public FuzzyFloat () {
  }

  // specify the specific relative difference allowed between two
  // floats in order for them to be equal.  The default is 0.0001
  // a relative diff of zero, disables it (ie, only exact matches work)
  public FuzzyFloat (double rel_diff) {
    set_rel_diff (rel_diff);
  }

  //set the relative diff after creation
  public void set_rel_diff (double rel_diff) {
    min_ratio = 1 - rel_diff;
    max_ratio = 1 + rel_diff;
    off = (rel_diff == 0.0);

  }

  // it seems like there ought to be a more efficient way to do this.  Since
  // we are going with percentages it seems that nothing is close enough
  // to zero to be zero (since the difference between the two is equal to
  // the non-zero one so the difference is exactly 100%
  //
  public boolean eq (double d1, double d2) {

    //these won't test as equal in a simple test or when divided
    if (Double.isNaN(d1) && Double.isNaN(d2))
      return (true);

    //if zero was specified for a ratio, don't do the divide.  You get slightly
    //different answers.
    if (off)
      return (d1 == d2);

    //only zero matches no matter what the ratio.  Saves on overflow checks
    //below as well
    if (d1 == 0.0)
      return (d2 == 0.0);
    else if (d2 == 0.0)
      return (d1 == 0.0);

    double ratio = d1/d2;
    return ((ratio >= min_ratio) && (ratio <= max_ratio));
  }

  //not equal test
  public boolean ne (double d1, double d2) {
   return (!eq (d1, d2));
  }

  //less than test
  public boolean lt (double d1, double d2) {
    return ((d1 < d2) && ne (d1, d2));
  }

  //less than or equal test
  public boolean lte (double d1, double d2) {
    return ((d1 <= d2) || eq (d1, d2));
  }

  //greater than test
  public boolean gt (double d1, double d2) {
      return ((d1 > d2) && ne (d1, d2));
  }

  //greater than or equal test
  public boolean gte (double d1, double d2) {
    return ((d1 >= d2) || eq (d1, d2));
  }

  /**
   * Searches for the first occurence of the given element in the array,
   *
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public int indexOf (double[] a, double elt) {
     for (int i=0; i<a.length; i++)
       if (eq (elt, a[i]))
        return i;
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the
   * given array elementwise.
   *
   * @return the first index whose subarray is equal to the specified array
   *    or -1 if no such subarray is found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public int indexOf (double[] a, double[] sub) {

    int a_index_max = a.length - sub.length;

    outer: for (int i = 0; i <= a_index_max; i++) {
      for (int j = 0; j < sub.length; j++) {
        if (ne (a[i+j], sub[j])) {
          continue outer;
        }
      }
      return (i);
    }
    return (-1);
  }

  /**
   * whether or not the two arrays (a1 and a2) contain the same elements
   * (ie, that each element in a1 is also in a2 and vice versa.
   *
   * Note that this implementation is optimized for cases where the
   * elements are actually the same, since it does a sort of both arrays
   * before starting the comparisons.
   *
   * @return true if a1 and a2 are set equivalent, false otherwise
   */
  public boolean isElemMatch (double[] a1, double[] a2) {

    Arrays.sort (a1);
    Arrays.sort (a2);

    // look for elements of a2 in a1
    int start = 0;
    outer1: for (int i = 0; i < a2.length; i++) {
      double val = a2[i];
      for (int j = start; j < a1.length; j++) {
        if (eq (val, a1[j])) {
          start = j;
          continue outer1;
        }
        if (val < a1[j]) {
          // System.out.println ("isElemMatch: " + val + " " + a1[j]);
          return (false);
        }
      }
      // System.out.println ("isElemMatch: " + i);
      return (false);
    }

    // look for elements of a1 in a2
    start = 0;
    outer2: for (int i = 0; i < a1.length; i++) {
      double val = a1[i];
      for (int j = start; j < a2.length; j++) {
        if (eq (val, a2[j])) {
          start = j;
          continue outer2;
        }
        if (val < a2[j]) {
          // System.out.println ("isElemMatch: " + val + " " + a2[j]);
          return (false);
        }
      }
      // System.out.println ("isElemMatch: " + i);
      return (false);
    }

    return (true);
  }

    // Slightly more efficient method that will miss some matches
//     int i = 0;
//     int j = 0;
//     while (i < a1.length && j < a2.length) {
//       if (ne (a1[i], a2[j])) {
//         System.out.println ("isElemMatch: " + a1[i] + " " + a2[j]);
//         return (false);
//       }
//       double val = a1[i];
//       i++;
//       while ((i < a1.length) && (eq (a1[i], val))) {
//         i++;
//       }
//       j++;
//       while ((j < a2.length) && (eq (a2[j], val))) {
//         j++;
//       }
//     }

//     // if there are any elements left, then they don't match.
//     if ((i != a1.length) || (j != a2.length)) {
//       System.out.println ("isElemMatch: " + i + " " + j);
//       return (false);
//     }

//     return (true);
//     }



  /**
   * Lexically compares o1 and o2 as double arrays.
   *
   * @return postive if o1 > 02, 0 if 01 == 02, negative if 01 < 02
   */

  public class DoubleArrayComparatorLexical implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      double[] a1 = (double[])o1;
      double[] a2 = (double[])o2;
      int len = Math.min(a1.length, a2.length);
      for (int i=0; i<len; i++) {
        if (ne (a1[i], a2[i])) {
          return ((a1[i] > a2[i]) ? 1 : -1);
        }
      }
      return a1.length - a2.length;
    }
  }


}
