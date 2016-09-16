package MapQuick.tigerdb;

import java.util.Arrays;

import junit.framework.Assert;

/**
 * Immutable ADT used to store street number sets while being read in
 * from tiger format.  Eventually, unparse() is called and that String
 * is passed off to userland.
 *
 * @author Jeremy Nimmer (ST01)
 */
public class IntSet
{
  private final int[][] elts = new int[2][];
  private final int[] span = new int[2];

  //
  // Rep Invariant
  //
  //  * for n in {0, 1}
  //    * 0 <= span[n] <= elts[n].length
  //    * The elements of elts[n] up to index span[n] are sorted and unique
  //    * The elements of elts[n] starting with index span[n] are MAXINT
  //

  public void checkRep()
  {
    for (int n=0; n<=1; n++) {
      int size = span[n];
      int[] nums = elts[n];

      Assert.assertTrue("0 <= span[n] <= elts[n].length", (0 <= size) && (size <= nums.length));

      for (int i=1; i<size; i++) {
        Assert.assertTrue("The elements of elts[n] up to index span[n] are unique", nums[i-1] != nums[i]);
        Assert.assertTrue("The elements of elts[n] up to index span[n] are sorted", nums[i-1] <= nums[i]);
      }

      for (int i=size; i<nums.length; i++) {
	Assert.assertEquals("The elements of elts[n] starting with index span[n] are MAXINT", nums[i], Integer.MAX_VALUE);
      }
    }
  }

  public IntSet()
  {
    elts[0] = elts[1] = EMPTY;
    span[0] = span[1] = 0;
  }
  private static final int[] EMPTY = new int[0];

  /**
   * @requires low, high >= 0 && low and high have the same parity &&
   * low <= high
   *
   * @effects Creates a new IntSet containing every other number from
   * low to high, inclusive
   */
  public IntSet(int low, int high)
  {
    this();

    if (low < 0 || high < 0) {
      throw new IllegalArgumentException("Negative numbers");
    }
    if (low > high) {
      throw new IllegalArgumentException("Bad direction");
    }
    int parity = parity(low);
    if (parity != parity(high)) {
      throw new IllegalArgumentException("Mismatched parity");
    }

    int size = span[parity] = 1 + ((high - low) / 2);
    int[] elements = elts[parity] = new int[size];

    int value = low;
    for (int i = 0; i < size; i++) {
      elements[i] = value;
      value += 2;
    }
    // value == high + 2
  }

  // @returns n in {0, 1} where n is the parity of i
  private static int parity(int i)
  {
    return i & 1;
  }

  /**
   * @requires other != null
   *
   * @returns an IntSet which is the set union of this and other
   */
  public IntSet union(IntSet other)
  {
    // handle degerate cases without blowing memory
    if (other.size() == 0) {
      return this;
    }
    if (this.size() == 0) {
      return other;
    }

    // int overlap = 0;

    // construct the union "result = this + other"
    IntSet result = new IntSet();
    for (int parity = 0; parity <= 1; parity++) {
      int[] a = this.elts[parity];
      int[] b = other.elts[parity];
      int anum = this.span[parity];
      int bnum = other.span[parity];

      // degerate case
      if (anum == 0) {
	result.elts[parity] = b;
	result.span[parity] = bnum;
	continue;
      }

      // degerate case
      if (bnum == 0) {
	result.elts[parity] = a;
	result.span[parity] = anum;
	continue;
      }

      // merge a + b into r
      // remove duplicates along the way
      int[] r = new int[anum + bnum];
      int ndummy = 0;
      int ai = 0, bi = 0;
      for (int ri = 0; ri < r.length; ri++) {
	boolean aok = (ai < anum);
	boolean bok = (bi < bnum);
	if (!aok && !bok) {
	  // we assume this will rarely happen, so it's not worth
	  // breaking this out into another loop
	  r[ri] = Integer.MAX_VALUE;
	  ndummy++;
	} else if (!aok) {
	  r[ri] = b[bi++];
	} else if (!bok) {
	  r[ri] = a[ai++];
	} else {
	  if (a[ai] == b[bi]) {
	    // advance both on a duplicate
	    r[ri] = a[ai];
	    ai++; bi++;
	    // overlap++;
	  } else if (a[ai] < b[bi]) {
	    // else just grab the lesser one
	    r[ri] = a[ai++];
	  } else {
	    r[ri] = b[bi++];
	  }
	}
      }
      result.elts[parity] = r;
      result.span[parity] = r.length - ndummy;
    }

    // Assert.assertTrue((this.size() + other.size()) == (result.size() + overlap));

    // result.checkRep();
    return result;
  }

  /**
   * @returns the number of elements in this
   */
  public int size()
  {
    return span[0] + span[1];
  }

  /**
   * @returns true iff i is in this
   */
  public boolean contains(int i)
  {
    return Arrays.binarySearch(elts[parity(i)], i) >= 0;
  }

  /**
   * @requires other != null
   * @returns true iff this and other are disjoint
   */
  public boolean isDisjoint(IntSet other)
  {
    return
      !intersects(this.elts[0], this.span[0], other.elts[0], other.span[0]) &&
      !intersects(this.elts[1], this.span[1], other.elts[1], other.span[1]);
  }

  // 0 <= xnum <= x.length
  private static boolean intersects(int[] a, int anum,
				    int[] b, int bnum)
  {
    // a should be the larger one
    if (anum < bnum) {
      int[] t = a;
      int tnum = anum;
      a = b;
      anum = bnum;
      b = t;
      bnum = tnum;
    }

    // O(b * log a)
    for (int bi = 0; bi < bnum; bi++) {
      int bx = b[bi];
      if (Arrays.binarySearch(a, bx) >= 0) {
	return true;
      }
    }

    return false;
  }

  /**
   * @returns result in the form:<pre>
   *
   * result := term |
   *           result ',' term
   *
   * term := number |
   *         number '-' number
   *
   * number := [0-9]+
   *
   * </pre>
   *
   * The na-nb productions in the result will have na, nb having the
   * same parity, and na < nb.  The result will have the minimal
   * number of terms possible (and therefore, the result will not
   * imply any number more than once).
   */
  public String unparse()
  {
    if (this.size() == 0) {
      return "";
    }

    StringBuffer result = new StringBuffer();

    for (int parity = 0; parity <= 1; parity++) {
      int[] elements = elts[parity];
      int size = span[parity];
      if (size > 0) {
	int range_start = 0;
	int cur = 1;
	while (cur < size) {
	  boolean broken = (elements[cur] != (elements[cur - 1] + 2));
	  if (broken) {
	    unparse(result, elements[range_start], elements[cur - 1]);
	    range_start = cur;
	  }
	  cur++;
	}
	unparse(result, elements[range_start], elements[size - 1]);
      }
    }

    // remove last ','
    result.setLength(result.length() - 1);

    return result.toString();
  }

  // "##-###," if begin != end
  // "###,"    if begin == end
  private static void unparse(StringBuffer result, int begin, int end)
  {
    result.append(begin);
    if (begin != end) {
      result.append('-');
      result.append(end);
    }
    result.append(',');
  }

}
