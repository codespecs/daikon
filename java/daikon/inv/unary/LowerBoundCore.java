package daikon.inv.unary;

import daikon.*;
import daikon.inv.*;
import daikon.derive.unary.*;

import java.util.*;

// *****
// Automatically generated from Bound.java.jpp
// *****

// One reason not to combine LowerBound and Upperbound is that they have
// separate justifications:  one may be justified when the other is not.

// What should we do if there are few values in the range?
// This can make justifying that invariant easier, because with few values
// naturally there are more instances of each value.
// This might also make justifying that invariant harder, because to get more
// than (say) twice the expected number of samples (under the assumption of
// uniform distribution) requires many samples.
// Which of these dominates?  Is the behavior what I want?

public class LowerBoundCore  implements java.io.Serializable {

  final static int required_samples_at_bound = 3;

  // min1  <  min2  <  min3 
  public long min1  = Long.MAX_VALUE ;
  int num_min1  = 0;
  long min2  = Long.MAX_VALUE ;
  int num_min2  = 0;
  long min3  = Long.MAX_VALUE ;
  int num_min3  = 0;
  long max  = Long.MIN_VALUE ;

  int samples = 0;

  Invariant wrapper;

  public LowerBoundCore (Invariant wrapper) {
    this.wrapper = wrapper;
  }

  public String repr() {
    return "min1=" + min1 
      + ", num_min1=" + num_min1 
      + ", min2=" + min2 
      + ", num_min2=" + num_min2 
      + ", min3=" + min3 
      + ", num_min3=" + num_min3 
      + ", max=" + max ;
  }

  public void add_modified(long value, int count) {
    samples += count;

    // System.out.println("LowerBoundCore"  + varNames() + ": "
    //                    + "add(" + value + ", " + modified + ", " + count + ")");

    long v = value;

    if (v >  max ) max  = v;

    if (v == min1 ) {
      num_min1  += count;
    } else if (v <  min1 ) {
      min3  = min2 ;
      num_min3  = num_min2 ;
      min2  = min1 ;
      num_min2  = num_min1 ;
      min1  = v;
      num_min1  = count;
    } else if (v == min2 ) {
      num_min2  += count;
    } else if (v <  min2 ) {
      min3  = min2 ;
      num_min3  = num_min2 ;
      min2  = v;
      num_min2  = count;
    } else if (v == min3 ) {
      num_min3  += count;
    } else if (v <  min3 ) {
      min3  = v;
      num_min3  = count;
    }
  }

  public double computeProbability() {
    if (num_min1  < required_samples_at_bound)
      return Invariant.PROBABILITY_UNKNOWN;

    long modulus = 1;
    // Need to reinstate this at some point.
    // {
    //   for (Iterator itor = wrapper.ppt.invs.iterator(); itor.hasNext(); ) {
    //     Invariant inv = (Invariant) itor.next();
    //     if ((inv instanceof Modulus) && inv.justified()) {
    //       modulus = ((Modulus) inv).modulus;
    //       break;
    //     }
    //   }
    // }

    // Accept a bound if:
    //  * it contains more than twice as many elements as it ought to by
    //    chance alone, and that number is at least 3.
    //  * it and its predecessor/successor both contain more than half
    //    as many elements as they ought to by chance alone, and at
    //    least 3.

    // If I used Math.abs, the order of arguments to minus would not matter.
    long range =  (max  - min1 ) + 1;
    double avg_samples_per_val = ((double) wrapper.ppt.num_mod_non_missing_samples()) * modulus / range;

    // System.out.println("  [Need to fix computation of LowerBoundCore.computeProbability()]");
    boolean truncated_justified = num_min1  > 5*avg_samples_per_val;
    if (truncated_justified) {
      return Invariant.PROBABILITY_JUSTIFIED;
    }

    boolean uniform_justified = (( (min3  - min2 ) == modulus)
                                 && ( (min2  - min1 ) == modulus)
                                 && (num_min1  > avg_samples_per_val/2)
                                 && (num_min2  > avg_samples_per_val/2)
                                 && (num_min3  > avg_samples_per_val/2));

    // System.out.println("LowerBoundCore.computeProbability(): ");
    // System.out.println("  " + repr_long());
    // System.out.println("  ppt=" + ppt
    //                    + ", ppt.num_mod_non_missing_samples()=" + ppt.num_mod_non_missing_samples()
    //                    + ", values=" + values
    //                    + ", avg_samples_per_val=" + avg_samples_per_val
    //                    + ", truncated_justified=" + truncated_justified
    //                    + ", uniform_justified=" + uniform_justified);
    // PptSlice pptsg = (PptSlice) ppt;
    // System.out.println("  " + ppt.name + " ppt.values_cache.tuplemod_samples_summary()="
    //                    + pptsg.tuplemod_samples_summary());

    if (uniform_justified)
      return Invariant.PROBABILITY_JUSTIFIED;

    return Invariant.PROBABILITY_UNJUSTIFIED;
  }

  public boolean isSameFormula(LowerBoundCore  other)
  {
    return min1  == other. min1 ;
  }

  public boolean isExact() {
    return false;
  }

}

