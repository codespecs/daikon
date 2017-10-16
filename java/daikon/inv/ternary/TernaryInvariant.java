package daikon.inv.ternary;

import daikon.PptSlice;
import daikon.inv.Invariant;
import daikon.inv.InvariantStatus;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import typequals.*;
*/

/** Exists simply to provide a more intelligent resusurrect_done method. */
public abstract class TernaryInvariant extends Invariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected TernaryInvariant(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ TernaryInvariant() {
    super();
  }

  // Check if swap occurred and call one of the other methods
  @Override
  protected Invariant resurrect_done(int[] permutation) {
    assert permutation.length == 3;
    // assert ArraysMDE.fn_is_permutation(permutation);
    throw new Error("to implement");
  }

  public abstract InvariantStatus add(
      /*@Interned*/ Object val1,
      /*@Interned*/ Object val2,
      /*@Interned*/ Object val3,
      int mod_index,
      int count);

  public abstract InvariantStatus check(
      /*@Interned*/ Object val1,
      /*@Interned*/ Object val2,
      /*@Interned*/ Object val3,
      int mod_index,
      int count);
}
