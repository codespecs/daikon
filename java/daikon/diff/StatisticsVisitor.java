package daikon.diff;

import utilMDE.Assert;
import daikon.inv.Invariant;

public class StatisticsVisitor implements NodeVisitor {
  
  private int identicalUnary = 0;
  private int missingUnary = 0;
  private int differingUnary = 0;

  private int identicalBinary = 0;
  private int missingBinary = 0;
  private int differingBinary = 0;

  private int identicalTernary = 0;
  private int missingTernary = 0;
  private int differingTernary = 0;

  public int getIdenticalUnary() {
    return identicalUnary;
  }
  public int getMissingUnary() {
    return missingUnary;
  }
  public int getDifferingUnary() {
    return differingUnary;
  }


  public int getIdenticalBinary() {
    return identicalBinary;
  }
  public int getMissingBinary() {
    return missingBinary;
  }
  public int getDifferingBinary() {
    return differingBinary;
  }


  public int getIdenticalTernary() {
    return identicalTernary;
  }
  public int getMissingTernary() {
    return missingTernary;
  }
  public int getDifferingTernary() {
    return differingTernary;
  }


  public void visitRootNode(RootNode node) { }

  public void visitPptNode(PptNode node) { }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    
    int arity = determineArity(inv1, inv2);
    
    if (inv1 == null || inv2 == null) {
      processMissing(inv1, inv2, arity);
    } else if (inv1.isSameInvariant(inv2)) {
      processIdentical(inv1, inv2, arity);
    } else {
      processDiffering(inv1, inv2, arity);
    }
  }

  private int determineArity(Invariant inv1, Invariant inv2) {
    // Use the a non-null invariant to determine arity
    Invariant i = (inv1 != null) ? inv1 : inv2;
    return i.ppt.arity;
  }

  private void processMissing(Invariant inv1, Invariant inv2, int arity) {
    switch(arity) {
    case 1:
      missingUnary++;
      break;
    case 2:
      missingBinary++;
      break;
    case 3:
      missingTernary++;
      break;
    default:
      Assert.assert(false, "can't get here");
      break;
    }
  }

  private void processIdentical(Invariant inv1, Invariant inv2, int arity) {
    switch(arity) {
    case 1:
      identicalUnary++;
      break;
    case 2:
      identicalBinary++;
      break;
    case 3:
      identicalTernary++;
      break;
    default:
      Assert.assert(false, "can't get here");
      break;
    }
  }

  private void processDiffering(Invariant inv1, Invariant inv2, int arity) {
    switch(arity) {
    case 1:
      differingUnary++;
      break;
    case 2:
      differingBinary++;
      break;
    case 3:
      differingTernary++;
      break;
    default:
      Assert.assert(false, "can't get here");
      break;
    }
  }
  
}
