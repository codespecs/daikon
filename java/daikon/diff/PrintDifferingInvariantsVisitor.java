package daikon.diff;

import daikon.inv.Invariant;

public class PrintDifferingInvariantsVisitor extends PrintAllVisitor {

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (invariantsDiffer(inv1, inv2)) {
      super.visitInvNode(node);
    }
  }

  protected static boolean invariantsDiffer(Invariant inv1, Invariant inv2) {
    if (inv1 == null || inv2 == null || !inv1.isSameInvariant(inv2)) {
      return true;
    }
    return false;
  }

}
