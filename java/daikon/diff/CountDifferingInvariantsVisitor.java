package daikon.diff;

import daikon.inv.Invariant;

public class CountDifferingInvariantsVisitor implements NodeVisitor {
  
  private int numDifferingInvariants = 0;

  public int getDifferingInvariantsCount() {
    return numDifferingInvariants;
  }

  public void visitRootNode(RootNode node) { }

  public void visitPptNode(PptNode node) { }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (invariantsDiffer(inv1, inv2)) {
      numDifferingInvariants++;
    }
  }

  protected static boolean invariantsDiffer(Invariant inv1, Invariant inv2) {
    if (inv1 == null || inv2 == null || !inv1.isSameInvariant(inv2)) {
      return true;
    }
    return false;
  }

}
