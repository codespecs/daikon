package daikon.inv.unary;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import org.checkerframework.checker.interning.qual.Interned;
import typequals.prototype.qual.Prototype;

/** Exists simply to provide the do-nothing resusurrect_done method and abstract add method. */
public abstract class UnaryInvariant extends Invariant {
  static final long serialVersionUID = 20020122L;

  protected UnaryInvariant(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype UnaryInvariant() {
    super();
  }

  @Override
  protected Invariant resurrect_done(int[] permutation) {
    assert permutation.length == 1;
    assert permutation[0] == 0;
    return this;
  }

  public abstract InvariantStatus add(@Interned Object val, int mod_index, int count);

  public abstract InvariantStatus check(@Interned Object val1, int mod_index, int count);
}
