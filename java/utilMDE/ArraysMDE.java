package utilMDE;

import java.lang.ref.WeakReference;
import java.util.*;


/**
 * Utilities for manipulating arrays.
 * This complements @link{java.util.Arrays}.
 */
public final class ArraysMDE {

  ///////////////////////////////////////////////////////////////////////////
  /// min, max
  ///

  // Could also add linear-time orderStatistics if I liked.

  /**
   * Return the smallest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static int min(int[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(int[])");
    int result = a[0];
    for (int i=1; i<a.length; i++)
      result = Math.min(result, a[i]);
    return result;
  }

  /**
   * Return the smallest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static Integer min(Integer[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Integer[])");
    Integer result = a[0];	// to return a value actually in the array
    int result_int = result.intValue();	// for faster comparison
    for (int i=1; i<a.length; i++) {
      if (a[i].intValue() < result_int) {
	result = a[i];
	result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static int max(int[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(int[])");
    int result = a[0];
    for (int i=1; i<a.length; i++)
      result = Math.max(result, a[i]);
    return result;
  }

  /**
   * Return the largest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static Integer max(Integer[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Integer[])");
    Integer result = a[0];	// to return a value actually in the array
    int result_int = result.intValue();	// for faster comparison
    for (int i=1; i<a.length; i++) {
      if (a[i].intValue() > result_int) {
	result = a[i];
	result_int = result.intValue();
      }
    }
    return result;
  }

  /**
   * Return a two-element array containing the smallest and largest values in the array.
   * Return null if the array has length 0.
   **/
  public static int[] min_max(int[] a) {
    if (a.length == 0)
      // throw new ArrayIndexOutOfBoundsException("Empty array passed to min_max(int[])");
      return null;
    int result_min = a[0];
    int result_max = a[0];
    for (int i=1; i<a.length; i++) {
      result_min = Math.min(result_min, a[i]);
      result_max = Math.max(result_max, a[i]);
    }
    return new int[] { result_min, result_max };
  }

  /**
   * Return the difference between the smallest and largest array elements.
   **/
  public static int element_range(int[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to element_range(int[])");
    int[] min_max = min_max(a);
    return min_max[1] - min_max[0];
  }

  // min, max for long (as opposed to int)

  /**
   * Return the smallest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static long min(long[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to min(long[])");
    long result = a[0];
    for (int i=1; i<a.length; i++)
      result = Math.min(result, a[i]);
    return result;
  }

  /**
   * Return the smallest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static Long min(Long[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Long[])");
    Long result = a[0];	// to return a value actually in the array
    long result_long = result.longValue();	// for faster comparison
    for (int i=1; i<a.length; i++) {
      if (a[i].longValue() < result_long) {
	result = a[i];
	result_long = result.longValue();
      }
    }
    return result;
  }

  /**
   * Return the largest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static long max(long[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(long[])");
    long result = a[0];
    for (int i=1; i<a.length; i++)
      result = Math.max(result, a[i]);
    return result;
  }

  /**
   * Return the largest value in the array.
   * @throws ArrayIndexOutOfBoundsException if the array has length 0
   **/
  public static Long max(Long[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to max(Long[])");
    Long result = a[0];	// to return a value actually in the array
    long result_long = result.longValue();	// for faster comparison
    for (int i=1; i<a.length; i++) {
      if (a[i].longValue() > result_long) {
	result = a[i];
	result_long = result.longValue();
      }
    }
    return result;
  }

  /**
   * Return a two-element array containing the smallest and largest values in the array.
   * Return null if the array has length 0.
   **/
  public static long[] min_max(long[] a) {
    if (a.length == 0)
      // throw new ArrayIndexOutOfBoundsException("Empty array passed to min_max(long[])");
      return null;
    long result_min = a[0];
    long result_max = a[0];
    for (int i=1; i<a.length; i++) {
      result_min = Math.min(result_min, a[i]);
      result_max = Math.max(result_max, a[i]);
    }
    return new long[] { result_min, result_max };
  }

  /**
   * Return the difference between the smallest and largest array elements.
   **/
  public static long element_range(long[] a) {
    if (a.length == 0)
      throw new ArrayIndexOutOfBoundsException("Empty array passed to element_range(long[])");
    long[] min_max = min_max(a);
    return min_max[1] - min_max[0];
  }


  // Returns the sum of an array of integers
  public static int sum(int[] a) {
    int sum = 0;
    for (int i = 0; i < a.length; i++) {
      sum += a[i];
    }
    return sum;
  }

  // Returns the sum of all the elements of a 2d array of integers
  public static int sum(int[][] a) {
    int sum = 0;
    for (int i = 0; i < a.length; i++) {
      for (int j = 0; j < a[0].length; j++) {
        sum += a[i][j];
      }
    }
    return sum;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// indexOf
  ///

  /**
   * Searches for the first occurence of the given element in the array,
   *    testing for equality using the equals method.
   * @return the first index whose element is equal to the specified element,
   *    or -1 if no such element is found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(Object[] a, Object elt) {
    for (int i=0; i<a.length; i++)
      if (elt.equals(a[i]))
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array,
   *    testing for equality using the equals method.
   * @return the first index i containing the specified element,
   *    such that minindex <= i < indexlimit,
   *    or -1 if the element is not found in that section of the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(Object[] a, Object elt, int minindex, int indexlimit) {
    for (int i=minindex; i<indexlimit; i++)
      if (elt.equals(a[i]))
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array,
   *    testing for equality using == (not the equals method).
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOfEq(Object[] a, Object elt) {
    for (int i=0; i<a.length; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array,
   *    testing for equality using == (not the equals method).
   * @return the first index i containing the specified element,
   *    such that minindex <= i < indexlimit,
   *    or -1 if the element is not found in that section of the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOfEq(Object[] a, Object elt, int minindex, int indexlimit) {
    for (int i=minindex; i<indexlimit; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(int[] a, int elt) {
    for (int i=0; i<a.length; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(long[] a, long elt) {
    for (int i=0; i<a.length; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index i containing the specified element,
   *    such that minindex <= i < indexlimit,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(int[] a, int elt, int minindex, int indexlimit) {
    for (int i=minindex; i<indexlimit; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index i containing the specified element,
   *    such that minindex <= i < indexlimit,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(long[] a, long elt, int minindex, int indexlimit) {
    for (int i=minindex; i<indexlimit; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(boolean[] a, boolean elt) {
    for (int i=0; i<a.length; i++)
      if (elt == a[i])
	return i;
    return -1;
  }

  /**
   * Searches for the first occurence of the given element in the array.
   * @return the first index i containing the specified element,
   *    such that minindex <= i < indexlimit,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   **/
  public static int indexOf(boolean[] a, boolean elt, int minindex, int indexlimit) {
    for (int i=minindex; i<indexlimit; i++)
      if (elt == a[i])
	return i;
    return -1;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// indexOf, for finding subarrays
  ///

  // This is analogous to Common Lisp's "search" function.

  // This implementation is very inefficient; I could use tricky Boyer-Moore
  // search techniques if I liked, but it's not worth it to me yet.


  /**
   * Searches for the first subsequence of the array that matches the given array elementwise,
   *    testing for equality using the equals method.
   * @return the first index whose element is equal to the specified element,
   *    or -1 if no such element is found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public static int indexOf(Object[] a, Object[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i=0; i<=a_index_max; i++)
      if (isSubarray(a, sub, i))
	return i;
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise,
   *    testing for equality using == (not the equals method).
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public static int indexOfEq(Object[] a, Object[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i=0; i<=a_index_max; i++)
      if (isSubarrayEq(a, sub, i))
	return i;
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public static int indexOf(int[] a, int[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i=0; i<=a_index_max; i++)
      if (isSubarray(a, sub, i))
	return i;
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public static int indexOf(long[] a, long[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i=0; i<=a_index_max; i++)
      if (isSubarray(a, sub, i))
	return i;
    return -1;
  }

  /**
   * Searches for the first subsequence of the array that matches the given array elementwise.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   * @see java.util.Vector#indexOf(java.lang.Object)
   * @see java.lang.String#indexOf(java.lang.String)
   **/
  public static int indexOf(boolean[] a, boolean[] sub) {
    int a_index_max = a.length - sub.length + 1;
    for (int i=0; i<=a_index_max; i++)
      if (isSubarray(a, sub, i))
	return i;
    return -1;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// mismatch
  ///

  // This is analogous to Common Lisp's "mismatch" function.

  // Put it off until later; for now, use the simpler subarray function,
  // which is a specialization of mismatch,


  ///////////////////////////////////////////////////////////////////////////
  /// subarray extraction
  ///

  // Note that the second argument is a length, not an end position.
  // That's to avoid confusion over whether it would be the last included
  // index or the first non-included index.

  public static Object[] subarray(Object[] a, int startindex, int length) {
    Object[] result = new Object[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  public static String[] subarray(String[] a, int startindex, int length) {
    String[] result = new String[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  public static byte[] subarray(byte[] a, int startindex, int length) {
    byte[] result = new byte[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  public static boolean[] subarray(boolean[] a, int startindex, int length) {
    boolean[] result = new boolean[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  public static char[] subarray(char[] a, int startindex, int length) {
    char[] result = new char[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  public static double[] subarray(double[] a, int startindex, int length) {
    double[] result = new double[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  // Note that the second argument is a length, not an end position.
  // (Is that the right design decision?)
  public static float[] subarray(float[] a, int startindex, int length) {
    float[] result = new float[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  // Note that the second argument is a length, not an end position.
  // (Is that the right design decision?)
  public static int[] subarray(int[] a, int startindex, int length) {
    int[] result = new int[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  // Note that the second argument is a length, not an end position.
  // (Is that the right design decision?)
  public static long[] subarray(long[] a, int startindex, int length) {
    long[] result = new long[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }

  // Note that the second argument is a length, not an end position.
  // (Is that the right design decision?)
  public static short[] subarray(short[] a, int startindex, int length) {
    short[] result = new short[length];
    System.arraycopy(a, startindex, result, 0, length);
    return result;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// subarray testing
  ///

  /**
   * Determines whether the second array is a subarray of the first,
   *    starting at the specified index of the first,
   *    testing for equality using the equals method.
   * @return the first index whose element is equal to the specified element,
   *    or -1 if no such element is found in the array.
   **/
  public static boolean isSubarray(Object[] a, Object[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len)
      return false;
    for (int i=0; i<sub_len; i++)
      if (! sub[i].equals(a[a_offset+i]))
	return false;
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first,
   *    starting at the specified index of the first,
   *    testing for equality using == (not the equals method).
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   **/
  public static boolean isSubarrayEq(Object[] a, Object[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len)
      return false;
    for (int i=0; i<sub_len; i++)
      if (sub[i] != a[a_offset+i])
	return false;
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first,
   *    starting at the specified index of the first.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   **/
  public static boolean isSubarray(int[] a, int[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len)
      return false;
    for (int i=0; i<sub_len; i++)
      if (sub[i] != a[a_offset+i])
	return false;
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first,
   *    starting at the specified index of the first.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   **/
  public static boolean isSubarray(long[] a, long[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len)
      return false;
    for (int i=0; i<sub_len; i++)
      if (sub[i] != a[a_offset+i])
	return false;
    return true;
  }

  /**
   * Determines whether the second array is a subarray of the first,
   *    starting at the specified index of the first.
   * @return the first index containing the specified element,
   *    or -1 if the element is not found in the array.
   **/
  public static boolean isSubarray(boolean[] a, boolean[] sub, int a_offset) {
    int a_len = a.length - a_offset;
    int sub_len = sub.length;
    if (a_len < sub_len)
      return false;
    for (int i=0; i<sub_len; i++)
      if (sub[i] != a[a_offset+i])
	return false;
    return true;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Printing
  ///

  // This should be extended to all types, when I get around to it.  The
  // methods are patterned after that of java.util.Vector (and use its
  // output format).

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toString(Object[] a) {
    return toString(a, false);
  }

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toStringQuoted(Object[] a) {
    return toString(a, true);
  }

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toString(Object[] a, boolean quoted) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i=1; i<a.length; i++) {
	sb.append(", ");
        if (quoted) {
          sb.append('\"');
          sb.append(UtilMDE.quote((String)a[i]));
          sb.append('\"');
        } else {
          sb.append(a[i]);
        }
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toString(int[] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i=1; i<a.length; i++) {
	sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toString(long[] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i=1; i<a.length; i++) {
	sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }

  /**
   * Return a string representation of the array.
   * The representation is patterned after that of java.util.Vector.
   * @see java.util.Vector#toString
   **/
  public static String toString(boolean[] a) {
    if (a == null) {
      return "null";
    }
    StringBuffer sb = new StringBuffer();
    sb.append("[");
    if (a.length > 0) {
      sb.append(a[0]);
      for (int i=1; i<a.length; i++) {
	sb.append(", ");
        sb.append(a[i]);
      }
    }
    sb.append("]");
    return sb.toString();
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Sortedness
  ///

  public static boolean sorted(int[] a) {
    for (int i=0; i<a.length-1; i++)
      if (a[i+1] < a[i])
	return false;
    return true;
  }

  public static boolean sorted(long[] a) {
    for (int i=0; i<a.length-1; i++)
      if (a[i+1] < a[i])
	return false;
    return true;
  }

  public static boolean sorted_descending(int[] a) {
    for (int i=0; i<a.length-1; i++)
      if (a[i+1] > a[i])
	return false;
    return true;
  }

  public static boolean sorted_descending(long[] a) {
    for (int i=0; i<a.length-1; i++)
      if (a[i+1] > a[i])
	return false;
    return true;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Array comparators
  ///

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical numbers).
   **/
  public static final class IntArrayComparatorLexical implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      int[] a1 = (int[])o1;
      int[] a2 = (int[])o2;
      int len = Math.min(a1.length, a2.length);
      for (int i=0; i<len; i++) {
        int tmp = a1[i] - a2[i];
        if (tmp != 0)
          return tmp;
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical numbers).
   **/
  public static final class LongArrayComparatorLexical implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      long[] a1 = (long[])o1;
      long[] a2 = (long[])o2;
      int len = Math.min(a1.length, a2.length);
      for (int i=0; i<len; i++) {
        long tmp = a1[i] - a2[i];
        if (tmp != 0) {
          if (tmp < Integer.MIN_VALUE) {
            return Integer.MIN_VALUE;
          } else if (tmp > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
          } else {
            return (int) tmp;
          }
        }
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical objects).
   **/
  public static final class ComparableArrayComparatorLexical implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      Comparable[] a1 = (Comparable[])o1;
      Comparable[] a2 = (Comparable[])o2;
      int len = Math.min(a1.length, a2.length);
      for (int i=0; i<len; i++) {
        int tmp = a1[i].compareTo(a2[i]);
        if (tmp != 0)
          return tmp;
        // Check the assumption that the two elements are equal.
        Assert.assert(a1[i].equals(a2[i]));
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical objects).
   **/
  public static final class ObjectArrayComparatorLexical implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      Object[] a1 = (Object[])o1;
      Object[] a2 = (Object[])o2;
      int len = Math.min(a1.length, a2.length);
      for (int i=0; i<len; i++) {
        int tmp = a1[i].hashCode() - a2[i].hashCode();
        if (tmp != 0)
          return tmp;
        // I'm counting on the fact that hashCode returns a different
        // number for each Object in the system.  This checks that assumption.
        Assert.assert(a1[i].equals(a2[i]));
      }
      return a1.length - a2.length;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical numbers).
   **/
  public static final class IntArrayComparatorLengthFirst implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      int[] a1 = (int[])o1;
      int[] a2 = (int[])o2;
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0)
        return tmp;
      for (int i=0; i<a1.length; i++) {
        tmp = a1[i] - a2[i];
        if (tmp != 0)
          return tmp;
      }
      return 0;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical numbers).
   **/
  public static final class LongArrayComparatorLengthFirst implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      long[] a1 = (long[])o1;
      long[] a2 = (long[])o2;
      int lendiff = a1.length - a2.length;
      if (lendiff != 0)
        return lendiff;
      long tmp;
      for (int i=0; i<a1.length; i++) {
        tmp = a1[i] - a2[i];
        if (tmp != 0) {
          if (tmp < Integer.MIN_VALUE) {
            return Integer.MIN_VALUE;
          } else if (tmp > Integer.MAX_VALUE) {
            return Integer.MAX_VALUE;
          } else {
            return (int) tmp;
          }
      }
      }
      return 0;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical objects).
   **/
  public static final class ComparableArrayComparatorLengthFirst implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      Comparable[] a1 = (Comparable[])o1;
      Comparable[] a2 = (Comparable[])o2;
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0)
        return tmp;
      for (int i=0; i<a1.length; i++) {
        tmp = a1[i].compareTo(a2[i]);
        if (tmp != 0)
          return tmp;
        // Check the assumption that the two elements are equal.
        Assert.assert(a1[i].equals(a2[i]));
      }
      return 0;
    }
  }

  /**
   * Note: this comparator imposes orderings that are inconsistent with equals.
   * That is, it may return 0 if the arrays are not equal (but do contain
   * identical objects).
   **/
  public static final class ObjectArrayComparatorLengthFirst implements Comparator {
    public int compare(Object o1, Object o2) {
      if (o1 == o2)
        return 0;
      Object[] a1 = (Object[])o1;
      Object[] a2 = (Object[])o2;
      int tmp;
      tmp = a1.length - a2.length;
      if (tmp != 0)
        return tmp;
      for (int i=0; i<a1.length; i++) {
        tmp = a1[i].hashCode() - a2[i].hashCode();
        if (tmp != 0)
          return tmp;
        // I'm counting on the fact that hashCode returns a different
        // number for each Object in the system.  This checks that assumption.
        Assert.assert(a1[i].equals(a2[i]));
      }
      return 0;
    }
  }


  ///////////////////////////////////////////////////////////////////////////
  /// javadoc hacks
  ///

  // this is so that javadoc can find "java.util.Vector".
  // "private static Vector v;" doesn't work, nor does
  // "static { new java.util.Vector(); }", nor does "private Vector v".
  // Yuck!
  public Vector javadocLossage;

}
