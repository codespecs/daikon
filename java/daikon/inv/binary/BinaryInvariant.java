package daikon.inv.binary;

import daikon.PptSlice;
import daikon.inv.Invariant;
import utilMDE.Assert;
import utilMDE.ArraysMDE;

/**
 * Exists simply to provide a more intelligent resusurrect_done method.
 **/
public abstract class BinaryInvariant
  extends Invariant
{
  /** Pass-through */
  protected BinaryInvariant(PptSlice ppt) {
    super(ppt);
  }

  // Check if swap occurred and call one of the other two methods
  protected Invariant resurrect_done(int[] permutation) {
    Assert.assert(permutation.length == 2);
    Assert.assert(ArraysMDE.fn_is_permutation(permutation));
    if (permutation[0] == 1)
      return resurrect_done_swapped();
    else
      return resurrect_done_unswapped();
  }

  /**
   * Do resurrect_done knowing that variables were swapped.
   **/
  abstract protected Invariant resurrect_done_swapped();

   /**
   * Subclasses can override in the rare cases they need to fix things
   * even when not swapped
   **/
  protected Invariant resurrect_done_unswapped() {
    // do nothing
    return this;
  }

}
