package daikon.inv;

import utilMDE.*;

public class IsEquality implements Filter {
  // Don't create new ones, just use this existing one
  public static final IsEquality it = new IsEquality();

  private IsEquality() { }
  public boolean accept(Object o) {
    Invariant inv = (Invariant) o;
    if (!(inv instanceof Comparison))
      return false;
    double chance_prob = ((Comparison) inv).eq_probability();
    return chance_prob < Invariant.probability_limit;
  }
}
