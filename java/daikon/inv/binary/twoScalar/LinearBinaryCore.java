package daikon.inv.binary.twoScalar;

import daikon.*;
import daikon.inv.*;
import utilMDE.*;

public final class LinearBinaryCore implements java.io.Serializable {

  final static boolean debugLinearBinaryCore = false;
  // final static boolean debugLinearBinaryCore = true;

  // y == ax + b; first argument is x, second is y
  public long a, b;

  Invariant wrapper;

  public int values_seen = 0;

  final static int MINPAIRS = 4;

  long[] x_cache = new long[MINPAIRS];
  long[] y_cache = new long[MINPAIRS];

  public LinearBinaryCore(Invariant wrapper) {
    this.wrapper = wrapper;
  }

  public void add_modified(long x, long y, int count) {
    if (values_seen < MINPAIRS) {
      // We delay computation of a and b until we have seen several pairs
      // so that we can compute a and b based on a far-separated pair.  If
      // the points in a pair are nearby, then roundoff errors in the
      // computation of the slope can be non-negligible.

      for (int i=0; i<values_seen; i++)
	if ((x_cache[i] == x) && (y_cache[i] == y))
	  return;
      x_cache[values_seen] = x;
      y_cache[values_seen] = y;
      values_seen++;
      if (values_seen == MINPAIRS) {
	// Find the most separated pair
        // Do I really need to check in two dimensions, or would one be enough?
        // indices of the most-separated pair
	int max_i = -1;
	int max_j = -1;
        // (square of the) distance between the most separated pair
	long max_separation = 0;
	for (int i=0; i<MINPAIRS-1; i++) {
	  for (int j=i+1; j<MINPAIRS; j++) {
            // not int, lest we get wraparound
	    long xsep = (x_cache[i] - x_cache[j]);
	    long ysep = (y_cache[i] - y_cache[j]);
	    long separation = xsep*xsep + ysep*ysep;
            // Assert.assert(separation > 0);
	    if (separation > max_separation) {
	      max_separation = separation;
	      max_i = i;
	      max_j = j;
	    }
	  }
	}
	// Set a and b based on that pair
	set_bi_linear(x_cache[max_i], x_cache[max_j], y_cache[max_i], y_cache[max_j]);
	// Check all values against a and b.
        if (!wrapper.no_invariant) {
          for (int i=0; i<MINPAIRS; i++) {
            // I should permit a fudge factor here.
            if (y_cache[i] != a*x_cache[i]+b) {
              if (debugLinearBinaryCore) {
                System.out.println("Suppressing " + "LinearBinaryCore" + " at index " + i + ": "
                                   + y_cache[i] + " != " + a + "*" + x_cache[i] + "+" + b);
              }
              wrapper.destroy();
              return;
            }
          }
        }
      }
    } else {
      // Check the new value against a and b.
      if (y != a*x+b) {
        if (debugLinearBinaryCore) {
          System.out.println("Suppressing " + "LinearBinaryCore" + " at new value: "
                             + y + " != " + a + "*" + x + "+" + b);
        }
        wrapper.destroy();
        return;
      }
    }
  }

  // Given ((x0,y0),(x1,y1)), set a and b such that y = ax + b.
  // If no such (a,b) exists, then destroy self.

  void set_bi_linear(long x0, long x1, long y0, long y1) {
    if (x0 == x1) {
      // x being constant would have been discovered elsewhere (and this
      // invariant would not have been instantiated).
      if (debugLinearBinaryCore) {
        System.out.println("Suppressing " + "LinearBinaryCore" + " due to equal x values: (" + x0 + "," + y0 + "), (" + x1 + "," + y1 + ")");
      }
      wrapper.destroy();
      return;
    }

    a = (y1-y0)/(x1-x0);
    b = (y0*x1-x0*y1)/(x1-x0);

  }

  public double computeProbability() {
    if (wrapper.no_invariant)
      return Invariant.PROBABILITY_NEVER;
    if (values_seen < MINPAIRS)
      return Invariant.PROBABILITY_UNKNOWN;
    return Invariant.PROBABILITY_JUSTIFIED;
  }

  public String repr() {
    return "LinearBinaryCore" + wrapper.varNames() + ": "
      + "a=" + a
      + ",b=" + b
      + ",values_seen=" + values_seen;
  }

  public static String format(String x, String y, long a, long b) {
    // For efficiency, I could use a single StringBuffer here.
    String b_rep = (b<0) ? (" - " + -b) : (b>0) ? (" + " + b) : "";
    String a_rep = (a==1) ? "" : ("" + a + " * ");
    return y + " == " + a_rep + x + b_rep;
  }

  public String format(String x, String y) {
    return format(x, y, a, b);
  }

  // Format as "x = cy+d" instead of as "y = ax+b".
  public String format_reversed(String x, String y) {
    Assert.assert(a == 1 || a == -1);
    return format(y, x, a, -b/a);
  }

  public boolean isSameFormula(LinearBinaryCore other)
  {
    boolean thisMeaningless = values_seen < MINPAIRS;
    boolean otherMeaningless = other.values_seen < MINPAIRS;

    if (thisMeaningless && otherMeaningless) {
      return true;
    } else {
      return
        (values_seen >= MINPAIRS) &&
        (other.values_seen >= MINPAIRS) &&
        (a == other.a) &&
        (b == other.b);
    }
  }

  public boolean isExclusiveFormula(LinearBinaryCore other)
  {
    if ((values_seen < MINPAIRS) ||
        (other.values_seen < MINPAIRS)) {
      return false;
    }

    return ((a == other.a)
            && (b != other.b));
  }

}

