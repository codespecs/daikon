package daikon.inv;

import utilMDE.*;

/** This Filter returns true if its argument is an Invariant which satisfies
 * the following conditions:
 * <pre>
 *  * the Invariant is a Comparison (which reports <, >, =, <=, or >=)
 *  * the relationship reported by the comparison is = (not <, <=, >, or >=)
 * </pre>
 * This does not consider PairwiseIntComparison to be an equality invariant.
 **/
public final class IsEqualityComparison implements Filter {

  // Don't create new instances, just use this existing one
  public final static IsEqualityComparison it = new IsEqualityComparison();

  private IsEqualityComparison() { }

  public boolean accept(Object o) {
    Assert.assertTrue(o instanceof Invariant);
    if (!(o instanceof Comparison))
      return false;
    double chance_prob = ((Comparison) o).eq_probability();
    return chance_prob < Invariant.dkconfig_probability_limit;
  }

  // Sadly, this does not work -- it conflicts with the member definition.
  // public static boolean accept(Object o) {
  //   return it.accept(o);
  // }
}
