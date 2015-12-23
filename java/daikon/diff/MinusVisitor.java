package daikon.diff;

import daikon.*;
import daikon.inv.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
*/

/**
 * Computes A - B, where A and B are the two sets of invariants.
 **/
public class MinusVisitor extends DepthFirstVisitor {

  private InvMap result = new InvMap();
  private /*@MonotonicNonNull*/ PptTopLevel currentPpt;

  /** If the first ppt is non-null, it should be part of the result. **/
  public void visit(PptNode node) {
    PptTopLevel ppt1 = node.getPpt1();
    if (ppt1 != null) {
      result.addPpt(ppt1);
      currentPpt = ppt1;
      super.visit(node);
    }
  }

  /** Possibly add the first invariant to the result set. **/
  @SuppressWarnings("nullness:contracts.precondition.override.invalid") // visitor invariant, because the PptNode has already been visited
  /*@RequiresNonNull("currentPpt")*/
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (shouldAdd(inv1, inv2)) {
      result.add(currentPpt, inv1);
    }
  }

  /**
   * If the first invariant is non-null and justified, and the second
   * one is null or unjustified, the first invariant should be added.
   **/
  /*@EnsuresNonNullIf(result=true, expression="#1")*/
  private static boolean shouldAdd(final /*@Nullable*/ Invariant inv1, /*@Nullable*/ Invariant inv2) {
    return ((inv1 != null) && (inv2 == null));
  }

  /** Returns the InvMap generated as a result of the traversal. **/
  public InvMap getResult() {
    return result;
  }

}
