package daikon.inv;

import daikon.*;

// An interface satisfied by IntComparison, SeqComparison,
// StringComparison, etc.  (Maybe that's the whole list.)
public interface Comparison {
  /**
   * If the invariant is a equality invariant, then its probability.
   * Otherwise, Invariant.PROBABILITY_NEVER.
   */
  public double eq_probability();

  public VarInfo var1();
  public VarInfo var2();
}
