package daikon.inv.twoScalar;

import daikon.*;
import daikon.inv.*;

class Linear extends TwoScalar {

  final static boolean debug_linear = false;

  // y == ax + b;
  int a, b;
  boolean no_invariant = false;

  int values_seen = 0;

  final static int MINPAIRS = 4;

  int[] x_array = new int[MINPAIRS];
  int[] y_array = new int[MINPAIRS];

  public Linear(PptSlice ppt_) {
    super(ppt_);
  }

//   public Linear(Ppt ppt_, VarInfo var_info1_, VarInfo var_info2_) {
//     super(ppt_, var_info1_, var_info2_);
//   }

  // Need to add these two methods for all subclasses of Invariant
  public String name() {
    return "Linear" + varNames();
  }
  public String long_name() {
    return name() + "@" + ppt.name;
  }

  public String repr() {
    double probability = getProbability();
    return "Linear" + varNames() + ": "
      + "no_invariant=" + no_invariant
      + ",a=" + a
      + ",b=" + b
      + "; probability = " + probability;
  }

  public String format() {
    if ((!no_invariant) && justified()) {
      String x = var1().name;
      String y = var2().name;
      String b_rep = (b<0) ? (" - " + -b) : (b>0) ? (" + " + b) : "";
      String a_rep = (a==1) ? "" : ("" + a + " * ");
      return y + " = " + a_rep + x + b_rep;
    } else {
      return null;
    }
  }

  public void add_modified(int x, int y, int count) {
    if (no_invariant) {
      return;
    }

    if (values_seen < MINPAIRS) {
      for (int i=0; i<values_seen; i++)
	if ((x_array[i] == i) && (y_array[i] == y))
	  return;
      x_array[values_seen] = x;
      y_array[values_seen] = y;
      values_seen++;
      if (values_seen == MINPAIRS) {
	// Find the most separated pair
	int max_separation = 0;
	int max_i = 0;
	int max_j = 1;
	for (int i=0; i<MINPAIRS-1; i++) {
	  for (int j=i+1; j<MINPAIRS; j++) {
	    int xsep = (x_array[i] - x_array[j]);
	    int ysep = (y_array[i] - y_array[j]);
	    int separation = xsep*xsep + ysep*ysep;
	    if (separation > max_separation) {
	      max_separation = separation;
	      max_i = i;
	      max_j = j;
	    }
	  }
	}
	// Set a and b based on that pair
	set_bi_linear(x_array[max_i], x_array[max_j], y_array[max_i], y_array[max_j]);
	// Check all values against a and b.
        if (!no_invariant) {
          for (int i=0; i<MINPAIRS; i++) {
            // I should permit a fudge factor here.
            if (y_array[i] != a*x_array[i]+b) {
              no_invariant = true;
              if (debug_linear) {
                System.out.println("Suppressing " + long_name() + " at index " + i + ": "
                                   + y_array[i] + " != " + a + "*" + x_array[i] + "+" + b);
              }
              break;
            }
          }
        }
      }
    } else {
      // Check the new value against a and b.
      if (y != a*x+b) {
	no_invariant = true;
        if (debug_linear) {
          System.out.println("Suppressing " + long_name() + " at new value: "
                             + y + " != " + a + "*" + x + "+" + b);
        }
      }
    }
  }


  // Given ((x0,y0),(x1,y1)), set a and b such that y = ax + b.
  // If no such (a,b) exists, then set no_invariant.

  void set_bi_linear(int x0, int x1, int y0, int y1) {
    if (x0 == x1) {
      no_invariant = true;
      if (debug_linear) {
        System.out.println("Suppressing " + long_name() + " due to equal x values: (" + x0 + "," + y0 + "), (" + x1 + "," + y1 + ")");
      }
      return;
    }

    double xdiff = x1-x0;

    // Assume that constants have already been found by a previous pass.
    a = (y1-y0)/(x1-x0);
    b = (y0*x1-x0*y1)/(x1-x0);

  }

  protected double computeProbability() {
    if (no_invariant)
      return Invariant.PROBABILITY_NEVER;
    if (values_seen < MINPAIRS)
      return Invariant.PROBABILITY_UNKNOWN;
    // This isn't right, is it?
    return 0;
  }


}
