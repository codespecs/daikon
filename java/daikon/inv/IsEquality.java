package daikon.inv;

import utilMDE.*;

// This Filter returns true if its argument is an Invariant which satisfies
// the following conditions:
//  1. the Invariant is a Comparison (which reports <, >, =, <=, or >=)
//  2. the relationship reported by the comparison is = (not <, <=, >, or >=)

public class IsEquality implements Filter {
  // Don't create new ones, just use this existing one
  public final static IsEquality it = new IsEquality();

  private IsEquality() { }
  public boolean accept(Object o) {
    // Gratuitous cast, but it does do some error-checking.
    Invariant inv = (Invariant) o;
    if (!(inv instanceof Comparison))
      return false;
    double chance_prob = ((Comparison) inv).eq_probability();
    return chance_prob < Invariant.probability_limit;
  }

  // Sadly, this does not work -- it conflicts with the member definition.
  // public static boolean accept(Object o) {
  //   return it.accept(o);
  // }
}
