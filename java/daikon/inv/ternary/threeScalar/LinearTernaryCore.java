package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.binary.twoScalar.LinearBinaryCore;
import utilMDE.*;

public final class LinearTernaryCore implements java.io.Serializable {

  final static boolean debugLinearTernaryCore = false;
  // final static boolean debugLinearTernaryCore = true;

  // z == ax + by + c; first argument is x, second is y, third is z
  public double a, b, c;

  Invariant wrapper;

  public int values_seen = 0;

  final static int MINTRIPLES = 5;

  // These are public to permit testing.
  public long[] x_cache = new long[MINTRIPLES];
  public long[] y_cache = new long[MINTRIPLES];
  public long[] z_cache = new long[MINTRIPLES];

  public LinearTernaryCore(Invariant wrapper) {
    this.wrapper = wrapper;
  }

  public void add_modified(long x, long y, long z, int count) {
    if (values_seen < MINTRIPLES) {
      // We delay computation of a and b until we have seen several triples
      // so that we can compute a and b based on a far-separated triple.  If
      // the points in a triple are nearby, then roundoff errors in the
      // computation of the slope can be non-negligible.

      for (int i=0; i<values_seen; i++)
	if ((x_cache[i] == x) && (y_cache[i] == y) && (z_cache[i] == z))
	  return;
      x_cache[values_seen] = x;
      y_cache[values_seen] = y;
      z_cache[values_seen] = z;
      values_seen++;
      if (values_seen == MINTRIPLES) {
	// Set a, b, and c based on an appropriate set of 3 (x, y, z) values.
        int[] maxsep_indices = maxsep_triples();
	set_tri_linear(maxsep_indices);

	// Check all values against a, b, and c.
        if (!wrapper.no_invariant) {
          // If one of these coefficients is zero, this should be a
          // LinearBinary, not a LinearTernary, term.  (It might not show up
          // as LinearBinary because there might not have been enough samples;
          // but a random varying third variable can create enough samples.)
          if ((a == 0) || (b == 0)) {
            wrapper.destroy();
            return;
          }

          for (int i=0; i<MINTRIPLES; i++) {
            // I should permit a fudge factor here.
            if (z_cache[i] != a*x_cache[i]+b*y_cache[i]+c) {
              if (debugLinearTernaryCore) {
                System.out.println("Suppressing " + "LinearTernaryCore" + " at index " + i + ": "
                                   + z_cache[i] + " != "
                                   + a + "*" + x_cache[i]
                                   + "+" + b + "*" + y_cache[i] + "+" + c);
              }
              wrapper.destroy();
              return;
            }
          }
        }
      }
    } else {
      // Check the new value against a, b, and c.
      if (z != a*x+b*y+c) {
        if (debugLinearTernaryCore) {
          System.out.println("Suppressing " + "LinearTernaryCore" + " at new value: "
                             + z + " != " + a + "*" + x + "+" + b + "*" + y + "+" + c + " = " + (a*x+b*y+c));
        }
        wrapper.destroy();
        return;
      }
    }
  }

  // Return the indices of three elements that are furthest from one another.
  int[] maxsep_triples() {
    // Set maxsep_i and maxsep_j to the indices of the most separated
    // triple.
    // Do I really need to check in two dimensions, or would one be enough?
    // indices of the most-separated triple
    int maxsep_i = -1;
    int maxsep_j = -1;
    int maxsep_k = -1;
    {
      // (square of the) distance between the most separated triple
      double max_separation = Double.MIN_VALUE;
      for (int i=0; i<MINTRIPLES-2; i++) {
        for (int j=i+1; j<MINTRIPLES-1; j++) {
          for (int k=j+1; j<MINTRIPLES; j++) {
            double separation = (separation(i, j)
                               + separation(i, k)
                               + separation(j, k));
            Assert.assert(separation > 0);
            if (separation > max_separation) {
              max_separation = separation;
              maxsep_i = i;
              maxsep_j = j;
              maxsep_k = k;
            }
          }
        }
      }
    }
    return new int[] { maxsep_i, maxsep_j, maxsep_k };
  }


  // (Square of the) distance between triples i and j.
  double separation(int i, int j) {
    // not int or even long, lest we get wraparound
    double xsep = (x_cache[i] - x_cache[j]);
    double ysep = (y_cache[i] - y_cache[j]);
    double zsep = (z_cache[i] - z_cache[j]);
    return xsep*xsep + ysep*ysep + zsep*zsep;
  }


  // Given ((x0,y0,z0),(x1,y1,z1)), set a, b, and c such that z = ax + by + c
  // If no such (a,b,c) exists, then destroy self.
  // Given a set of equations
  //    z0 = a x0 + b y0 + c
  //    z1 = a x1 + b y1 + c
  //    z2 = a x2 + b y2 + c
  // where (x, y, z) are known and we are solving for (a, b, c):
  //      | z0 y0 1 |
  //      | z1 y1 1 |
  //      | z2 y2 1 |
  // a = -------------
  //      | x0 y0 1 |
  //      | x1 y1 1 |
  //      | x2 y2 1 |
  //
  //      | x0 z0 1 |
  //      | x1 z1 1 |
  //      | x2 z2 1 |
  // b = -------------
  //      | x0 y0 1 |
  //      | x1 y1 1 |
  //      | x2 y2 1 |
  //
  //      | x0 y0 z0 |
  //      | x1 y1 z1 |
  //      | x2 y2 z2 |
  // c = -------------
  //      | x0 y0 1 |
  //      | x1 y1 1 |
  //      | x2 y2 1 |
  //
  // This is only public to permit testing.
  public void set_tri_linear(int[] indices) {
    int i0 = indices[0];
    int i1 = indices[1];
    int i2 = indices[2];
    long x0 = x_cache[i0]; long y0 = y_cache[i0]; long z0 = z_cache[i0];
    long x1 = x_cache[i1]; long y1 = y_cache[i1]; long z1 = z_cache[i1];
    long x2 = x_cache[i2]; long y2 = y_cache[i2]; long z2 = z_cache[i2];
    float denominator = (float) ((x1 * y2 - x2 * y1)
                       - (x0 * y2 - x2 * y0)
                       + (x0 * y1 - x1 * y0));
    float a_numerator = (float) ((z1 * y2 - z2 * y1)
                                - (z0 * y2 - z2 * y0)
                                + (z0 * y1 - z1 * y0));
    float b_numerator = (float) ((x1 * z2 - x2 * z1)
                                - (x0 * z2 - x2 * z0)
                                + (x0 * z1 - x1 * z0));
    float c_numerator = (float) (z0 * (x1 * y2 - x2 * y1)
                                - z1 * (x0 * y2 - x2 * y0)
                                + z2 * (x0 * y1 - x1 * y0));
    if (denominator == 0) {
      if (debugLinearTernaryCore) {
        System.out.println("Suppressing " + "LinearTernaryCore" + " due to zero denominator.");
      }
      wrapper.destroy();
      return;
    }
    try {
      a = a_numerator / denominator;
      b = b_numerator / denominator;
      c = c_numerator / denominator;
    } catch (Exception e) {
      if (debugLinearTernaryCore) {
        System.out.println("Suppressing " + "LinearTernaryCore" + " due to exception.");
      }
      wrapper.destroy();
      return;
    }
  }

// The old (Python) code that computed a, b, and c looked like this:
//     (x1, y1, z1) = triple1
//     (x2, y2, z2) = triple2
//     (x3, y3, z3) = triple3
//     # Possibly reorder the triples to avoid division-by-zero problems.
//     if (y2 == y3) or (x2 == x3):
//         return (0,0,0)
//     try:
//         y1323 = float(y1-y3)/(y2-y3)
//         a_numerator = z3-z1+(z2-z3)*y1323
//         a_denominator = x3-x1+(x2-x3)*y1323
//
//         x1323 = float(x1-x3)/(x2-x3)
//         b_numerator = z3-z1+(z2-z3)*x1323
//         b_denominator = y3-y1+(y2-y3)*x1323
//     except OverflowError:
//         return (0,0,0)
//     if (a_denominator == 0) or (b_denominator == 0):
//         return (0,0,0)
//     a = a_numerator/a_denominator
//     b = b_numerator/b_denominator
//     c = z3-a*x3-b*y3



  public boolean enoughSamples() {
    return values_seen >= MINTRIPLES;
  }

  public double computeProbability() {
    if (wrapper.no_invariant)
      return Invariant.PROBABILITY_NEVER;
    if (values_seen < MINTRIPLES)
      return Invariant.PROBABILITY_UNKNOWN;
    return Invariant.PROBABILITY_JUSTIFIED;
  }

  public String repr() {
    return "LinearTernaryCore" + wrapper.varNames() + ": "
      + "a=" + a
      + ",b=" + b
      + ",values_seen=" + values_seen;
  }

  // In this class for convenience (avoid prefixing "LinearBinaryCore").
  static String formatTerm(double coeff, String var, boolean first) {
    return LinearBinaryCore.formatTerm(coeff, var, first);
  }

  public static String format(String x, String y, String z, double a, double b, double c) {
    // It shouldn't be the case that a or b is 0 for printed invariants;
    // but that can be true earlier on in processing.
    if ((a == 0) && (b == 0) && (c == 0)) {
      return z + " == 0 * " + x + " + 0 * " + y + " + 0";
    }
    return z + " == " + formatTerm(a, x, true) + formatTerm(b, y, false) + formatTerm(c, null, false);
  }

  public String format(String x, String y, String z) {
    return format(x, y, z, a, b, c);
  }


  // // Format as "x = cy+d" instead of as "y = ax+b".
  // public String format_reversed(String x, String y) {
  //   Assert.assert(a == 1 || a == -1);
  //   return format(y, x, a, -b/a);
  // }

  public boolean isSameFormula(LinearTernaryCore other)
  {
    boolean thisMeaningless = values_seen < MINTRIPLES;
    boolean otherMeaningless = other.values_seen < MINTRIPLES;

    if (thisMeaningless && otherMeaningless) {
      return true;
    } else {
      return ((values_seen >= MINTRIPLES)
              && (other.values_seen >= MINTRIPLES)
              && (a == other.a)
              && (b == other.b)
              && (c == other.c));
    }
  }

  public boolean isExclusiveFormula(LinearTernaryCore other)
  {
    if ((values_seen < MINTRIPLES) ||
        (other.values_seen < MINTRIPLES)) {
      return false;
    }

    return ((a == other.a)
            && (b != other.b)
            && (c != other.c));
  }

}
