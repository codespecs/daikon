package daikon.inv;

import plume.*;

/**
 * This Filter returns true if its argument is an Invariant which satisfies the following
 * conditions:
 *
 * <ul>
 *   <li>the Invariant is an EqualityComparison (its relationship is =, not &lt;, &le;, &gt;, or
 *       &ge;).
 *   <li>the invariant is statistically satisfied (its confidence is above the limit)
 * </ul>
 *
 * This does not consider PairwiseIntComparison to be an equality invariant.
 */
public final class IsEqualityComparison implements Filter<Invariant> {

  // Don't create new instances, just use this existing one
  public static final IsEqualityComparison it = new IsEqualityComparison();

  private IsEqualityComparison() {}

  @Override
  public boolean accept(Invariant inv) {
    if (!(inv instanceof EqualityComparison)) return false;
    double chance_conf = ((EqualityComparison) inv).eq_confidence();
    return chance_conf > Invariant.dkconfig_confidence_limit;
  }

  // Sadly, this does not work -- it conflicts with the member definition.
  // public static boolean accept(Object o) {
  //   return it.accept(o);
  // }
}
