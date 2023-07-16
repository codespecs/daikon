package daikon.inv.ternary;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;
import org.checkerframework.checker.interning.qual.Interned;
import typequals.prototype.qual.Prototype;

/** Exists simply to provide a more intelligent {@link #resurrect_done} method. */
public abstract class TernaryInvariant extends Invariant {
  static final long serialVersionUID = 20020122L;

  protected TernaryInvariant(PptSlice ppt) {
    super(ppt);
  }

  protected @Prototype TernaryInvariant() {
    super();
  }

  // Check if swap occurred and call one of the other methods
  @Override
  protected Invariant resurrect_done(int[] permutation) {
    assert permutation.length == 3;
    // assert ArraysPlume.fn_is_permutation(permutation);
    throw new Error("to implement");
  }

  public abstract InvariantStatus add(
      @Interned Object val1,
      @Interned Object val2,
      @Interned Object val3,
      int mod_index,
      int count);

  public abstract InvariantStatus check(
      @Interned Object val1,
      @Interned Object val2,
      @Interned Object val3,
      int mod_index,
      int count);
}
