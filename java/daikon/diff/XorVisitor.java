package daikon.diff;

import java.util.*;
import daikon.*;
import daikon.inv.*;

/**
 * Computes A xor B, where A and B are the two sets of invariants.
 **/
public class XorVisitor extends DepthFirstVisitor {

  private InvMap result = new InvMap();
  private PptTopLevel currentPpt;

  /**
   * Every node has at least one non-null ppt.  Add one of the
   * non-null ppt to the result.
   **/
  public void visit(PptNode node) {
    PptTopLevel ppt1 = node.getPpt1();
    PptTopLevel ppt2 = node.getPpt2();
    PptTopLevel pptNonNull = (ppt1 != null ? ppt1 : ppt2);
    result.put(pptNonNull, new ArrayList());
    currentPpt = pptNonNull;
    super.visit(node);
  }

  /**
   * If one invariant is null and the other is not, add the non-null
   * invariant to the result set.
   **/
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (shouldAddInv1(inv1, inv2)) {
      result.get(currentPpt).add(inv1);
    } else if (shouldAddInv2(inv1, inv2)) {
      result.get(currentPpt).add(inv2);      
    }
  }


  private static boolean shouldAddInv1(Invariant inv1, Invariant inv2) {
    return ((inv1 != null && inv1.justified()) &&
            (inv2 == null || !inv2.justified()));
  }

  private static boolean shouldAddInv2(Invariant inv1, Invariant inv2) {
    return ((inv2 != null && inv2.justified()) &&
            (inv1 == null || !inv1.justified()));
  }
  

  /**
   * Returns the InvMap generated as a result of the traversal.
   **/
  public InvMap getResult() {
    return result;
  }

}
