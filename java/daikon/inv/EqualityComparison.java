package daikon.inv;

import daikon.*;

/** An interface satisfied by equality invariants such as {@code =}. */
public interface EqualityComparison {
  /** The confidence of this equality invariant. */
  public double eq_confidence();

  public VarInfo var1();

  public VarInfo var2();
}
