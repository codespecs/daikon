package daikon.diff;

import utilMDE.Assert;
import daikon.inv.*;
import daikon.inv.unary.*;
import daikon.inv.unary.scalar.*;
import java.io.*;

public class StatisticsVisitor implements NodeVisitor {
  
  private int identicalNullary = 0;
  private int missingNullary = 0;
  private int differingInterestingNullary = 0;
  private int differingUninterestingNullary = 0;

  private int identicalUnary = 0;
  private int missingUnary = 0;
  private int differingInterestingUnary = 0;
  private int differingUninterestingUnary = 0;

  private int identicalBinary = 0;
  private int missingBinary = 0;
  private int differingInterestingBinary = 0;
  private int differingUninterestingBinary = 0;

  private int identicalTernary = 0;
  private int missingTernary = 0;
  private int differingInterestingTernary = 0;
  private int differingUninterestingTernary = 0;

  public int getIdenticalNullary() {
    return identicalNullary;
  }
  public int getMissingNullary() {
    return missingNullary;
  }
  public int getDifferingNullary() {
    return differingInterestingNullary + differingUninterestingNullary;
  }
  public int getDifferingInterestingNullary() {
    return differingInterestingNullary;
  }
  public int getDifferingUninterestingNullary() {
    return differingUninterestingNullary;
  }

  public int getIdenticalUnary() {
    return identicalUnary;
  }
  public int getMissingUnary() {
    return missingUnary;
  }
  public int getDifferingUnary() {
    return differingInterestingUnary + differingUninterestingUnary;
  }
  public int getDifferingInterestingUnary() {
    return differingInterestingUnary;
  }
  public int getDifferingUninterestingUnary() {
    return differingUninterestingUnary;
  }


  public int getIdenticalBinary() {
    return identicalBinary;
  }
  public int getMissingBinary() {
    return missingBinary;
  }
  public int getDifferingBinary() {
    return differingInterestingBinary + differingUninterestingBinary;
  }
  public int getDifferingInterestingBinary() {
    return differingInterestingBinary;
  }
  public int getDifferingUninterestingBinary() {
    return differingUninterestingBinary;
  }


  public int getIdenticalTernary() {
    return identicalTernary;
  }
  public int getMissingTernary() {
    return missingTernary;
  }
  public int getDifferingTernary() {
    return differingInterestingTernary + differingUninterestingTernary;
  }
  public int getDifferingInterestingTernary() {
    return differingInterestingTernary;
  }
  public int getDifferingUninterestingTernary() {
    return differingUninterestingTernary;
  }

  // The total number of missing invariants
  public int getMissing() {
    return getMissingNullary() + getMissingUnary() +
      getMissingBinary() + getMissingTernary();
  }

  // The total number of differing invariants
  public int getDiffering() {
    return getDifferingNullary() + getDifferingUnary() +
      getDifferingBinary() + getDifferingTernary();
  }


  public void visitRootNode(RootNode node) { }

  public void visitPptNode(PptNode node) { }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    
    if (inv1 == null || inv2 == null) {
      processMissing(inv1, inv2);
    } else if (inv1.isSameInvariant(inv2)) {
      processIdentical(inv1, inv2);
    } else {
      processDiffering(inv1, inv2);
    }
  }

  private int determineArity(Invariant inv1, Invariant inv2) {
    // Use the non-null invariant to determine arity
    Invariant i = (inv1 != null) ? inv1 : inv2;
    return i.ppt.arity;
  }

  private void processMissing(Invariant inv1, Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    switch(arity) {
    case 0:
      missingNullary++;
      break;
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
      Assert.assert(false, "Invalid arity: " + arity);
      break;
    }
  }

  private void processIdentical(Invariant inv1, Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    switch(arity) {
    case 0:
      identicalNullary++;
      break;
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
      Assert.assert(false, "Invalid arity: " + arity);
      break;
    }
  }

  private void processDiffering(Invariant inv1, Invariant inv2) {
    if (interestingDifference(inv1, inv2)) {
      processInterestingDiffering(inv1, inv2);
    } else {
      processUninterestingDiffering(inv1, inv2);
    }
  }

  private void processInterestingDiffering(Invariant inv1, Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    switch(arity) {
    case 0:
      differingInterestingNullary++;
      break;
    case 1:
      differingInterestingUnary++;
      break;
    case 2:
      differingInterestingBinary++;
      break;
    case 3:
      differingInterestingTernary++;
      break;
    default:
      Assert.assert(false, "Invalid arity: " + arity);
      break;
    }
  }

  private void processUninterestingDiffering(Invariant inv1, Invariant inv2) {
    int arity = determineArity(inv1, inv2);
    switch(arity) {
    case 0:
      differingUninterestingNullary++;
      break;
    case 1:
      differingUninterestingUnary++;
      break;
    case 2:
      differingUninterestingBinary++;
      break;
    case 3:
      differingUninterestingTernary++;
      break;
    default:
      Assert.assert(false, "Invalid arity: " + arity);
      break;
    }
  }


  // Returns true if the difference between the two invariants is
  // considered "interesting".  All differences are interesting,
  // except: LowerBound, UpperBound, OneOf
  static boolean interestingDifference(Invariant inv1,
                                       Invariant inv2) {
    // Use the non-null invariant
    Invariant inv = (inv1 != null) ? inv1 : inv2;
    if (inv instanceof LowerBound ||
        inv instanceof UpperBound ||
        inv instanceof OneOf) {
      return false;
    }
    return true;
  }

  public String format() {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    
    pw.println("Identical nullary:   " + getIdenticalNullary());
    pw.println("Missing nullary:     " + getMissingNullary());
    pw.println("Differing nullary:   " + getDifferingNullary());
    pw.println("  interesting:       " + getDifferingInterestingNullary());
    pw.println("  uninteresting:     " + getDifferingUninterestingNullary());
    pw.println();
    pw.println("Identical unary:     " + getIdenticalUnary());
    pw.println("Missing unary:       " + getMissingUnary());
    pw.println("Differing unary:     " + getDifferingUnary());
    pw.println("  interesting:       " + getDifferingInterestingUnary());
    pw.println("  uninteresting:     " + getDifferingUninterestingUnary());
    pw.println();
    pw.println("Identical binary:    " + getIdenticalBinary());
    pw.println("Missing binary:      " + getMissingBinary());
    pw.println("Differing binary:    " + getDifferingBinary());
    pw.println("  interesting:       " + getDifferingInterestingBinary());
    pw.println("  uninteresting:     " + getDifferingUninterestingBinary());
    pw.println();
    pw.println("Identical ternary:   " + getIdenticalTernary());
    pw.println("Missing ternary:     " + getMissingTernary());
    pw.println("Differing ternary:   " + getDifferingTernary());
    pw.println("  interesting:       " + getDifferingInterestingTernary());
    pw.println("  uninteresting:     " + getDifferingUninterestingTernary());

    return sw.toString();
  }
}
