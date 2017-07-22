package daikon.inv.unary;

import daikon.PptSlice;
import daikon.inv.*;
import daikon.inv.InvariantStatus;

/*>>>
import org.checkerframework.checker.interning.qual.*;
import typequals.*;
*/

/** Exists simply to provide the do-nothing resusurrect_done method and abstract add method. */
public abstract class UnaryInvariant extends Invariant {
  // We are Serializable, so we specify a version to allow changes to
  // method signatures without breaking serialization.  If you add or
  // remove fields, you should change this number to the current date.
  static final long serialVersionUID = 20020122L;

  protected UnaryInvariant(PptSlice ppt) {
    super(ppt);
  }

  protected /*@Prototype*/ UnaryInvariant() {
    super();
  }

  /** @return this */
  @Override
  protected Invariant resurrect_done(int[] permutation) {
    assert permutation.length == 1;
    assert permutation[0] == 0;
    return this;
  }

  public abstract InvariantStatus add(/*@Interned*/ Object val, int mod_index, int count);

  public abstract InvariantStatus check(/*@Interned*/ Object val1, int mod_index, int count);
}
