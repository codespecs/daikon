package daikon.inv;

import daikon.*;

/** An interface satisfied by {@code <}, {@code <=}, {@code =}, etc. */
public interface Comparison {
  /**
   * If the invariant is a equality invariant, then its confidence. Otherwise,
   * Invariant.CONFIDENCE_NEVER.
   */
  public double eq_confidence();

  public VarInfo var1();

  public VarInfo var2();
}
