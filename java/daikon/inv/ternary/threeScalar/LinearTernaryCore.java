package daikon.inv.ternary.threeScalar;

import daikon.*;
import daikon.inv.*;
import daikon.inv.Invariant.OutputFormat;
import daikon.inv.binary.twoScalar.LinearBinaryCore;
import utilMDE.*;
import java.io.Serializable;

import org.apache.log4j.Category;

public final class LinearTernaryCore
  implements Serializable, Cloneable
{
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  /**
   * Debug tracer
   **/
  final static Category debug = Category.getInstance ("daikon.inv.trinary.threeScalar.LinearTernaryCore");

  // z == ax + by + c; first argument is x, second is y, third is z
  public double a, b, c;

  public Invariant wrapper;

  public int values_seen = 0;

  final static int MINTRIPLES = 5;

  // These are public to permit testing.
  public long[] x_cache = new long[MINTRIPLES];
  public long[] y_cache = new long[MINTRIPLES];
  public long[] z_cache = new long[MINTRIPLES];

  public LinearTernaryCore(Invariant wrapper) {
    this.wrapper = wrapper;
  }

  public Object clone() {
    try {
      LinearTernaryCore result = (LinearTernaryCore) super.clone();
      result.x_cache = (long[]) x_cache.clone();
      result.y_cache = (long[]) y_cache.clone();
      result.z_cache = (long[]) z_cache.clone();
      return result;
    } catch (CloneNotSupportedException e) {
      throw new Error(); // can't happen
    }
  }

  /**
   * Reorganize our already-seen state as if the variables had shifted
   * order underneath us (rearrangement given by the permutation).
   **/
  public void permute(int[] permutation) {
    Assert.assert(permutation.length == 3);
    Assert.assert(ArraysMDE.fn_is_permutation(permutation));
    // Fix a, b, c
    // clever because a*v0 + b*v1 - v2 = -c
    double[] clever = new double[] { a, b, -1.0 };
    double[] pclever = new double[3];
    pclever[permutation[0]] = clever[0];
    pclever[permutation[1]] = clever[1];
    pclever[permutation[2]] = clever[2];
    if (pclever[2] == 0) {
      // We can't handle this form.  Need to change "z = ax + by + c"
      // style into "az + by + cz = 1", so that we can zero out
      // any term we wish.
      values_seen = 0;
      a = b = c = 0;
      System.err.println("Warning; ternary invariant had a bad day.");
    } else {
      double d = -1.0 / pclever[2];
      a = pclever[0] * d;
      b = pclever[1] * d;
      c = c * d;
    }
    // Fix caches
    long[][] caches = new long[3][];
    caches[permutation[0]] = x_cache;
    caches[permutation[1]] = y_cache;
    caches[permutation[2]] = z_cache;
    x_cache = caches[0];
    y_cache = caches[1];
    z_cache = caches[2];
    // Could assert that caches sync with a,b,c (?)
  }

  public void add_modified(long x, long y, long z, int count)
  {
    // XXX TODO FIXME: This method does not correctly deal with
    // calling addToFlow.  A proper clone, etc. should be created and
    // dealt with.  This will complicate the logic below, I'll bet.

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
        if (!wrapper.falsified) {
          // If one of these coefficients is zero, this should be a
          // LinearBinary, not a LinearTernary, term.  (It might not show up
          // as LinearBinary because there might not have been enough samples;
          // but a random varying third variable can create enough samples.)
          /* [INCR] this makes stuff to weird
          if ((a == 0) || (b == 0)) {
            wrapper.destroy();
            return;
          }
          */

          for (int i=0; i<MINTRIPLES; i++) {
            // I should permit a fudge factor here.
            if (z_cache[i] != a*x_cache[i]+b*y_cache[i]+c) {
              if (debug.isDebugEnabled()) {
                debug.debug("Suppressing " + "LinearTernaryCore" + " at index " + i + ": "
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
        if (debug.isDebugEnabled()) {
          debug.debug("Suppressing " + "LinearTernaryCore" + " at new value: "
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
      if (debug.isDebugEnabled()) {
        debug.debug("Suppressing " + "LinearTernaryCore" + " due to zero denominator.");
      }
      wrapper.destroy();
      return;
    }
    try {
      a = a_numerator / denominator;
      b = b_numerator / denominator;
      c = c_numerator / denominator;
    } catch (Exception e) {
      if (debug.isDebugEnabled()) {
        debug.debug("Suppressing " + "LinearTernaryCore" + " due to exception.");
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
    if (wrapper.falsified)
      return Invariant.PROBABILITY_NEVER;
    return Invariant.prob_is_ge(values_seen, MINTRIPLES);
  }

  public String repr() {
    return "LinearTernaryCore" + wrapper.varNames() + ": "
      + "a=" + a
      + ",b=" + b
      + ",c=" + c
      + ",values_seen=" + values_seen;
  }

  public static String format_using(OutputFormat format,
                                    VarInfoName x, VarInfoName y, VarInfoName z,
                                    double a, double b, double c)
  {
    String xname = x.name_using(format);
    String yname = y.name_using(format);
    String zname = z.name_using(format);

    if ((format == OutputFormat.DAIKON)
        || (format == OutputFormat.ESCJAVA)
        || (format == OutputFormat.IOA)
        || (format == OutputFormat.JML))
    {
      String eq = " == ";
      if (format == OutputFormat.IOA) eq = " = ";

      // It shouldn't be the case that a or b is 0 for printed invariants;
      // but that can be true earlier on in processing.
      if ((a == 0) && (b == 0) && (c == 0)) {
        return zname + eq + "(? * " + xname + ") + (? * " + yname + ") + ?";
      }

      return zname + " == "
        + LinearBinaryCore.formatTerm(format, a, x, true)
        + LinearBinaryCore.formatTerm(format, b, y, false)
        + LinearBinaryCore.formatTerm(format, c, null, false);
    }

    return null;
  }

  public String format_using(OutputFormat format,
                             VarInfoName x, VarInfoName y, VarInfoName z)
  {
    String result = format_using(format, x, y, z, a, b, c);
    if (result != null) {
      return result;
    }

    return wrapper.format_unimplemented(format);
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
