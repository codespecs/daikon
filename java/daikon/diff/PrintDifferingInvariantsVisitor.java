package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;
import java.text.*;

public class PrintDifferingInvariantsVisitor extends PrintAllVisitor {

  private boolean printUninteresting;
  private boolean considerJustification;

  private static DecimalFormat PROBABILITY_FORMAT =
    new DecimalFormat("0.####");

  public PrintDifferingInvariantsVisitor(PrintStream ps,
                                         boolean verbose,
                                         boolean printUninteresting,
                                         boolean considerJustification) {
    super(ps, verbose);
    this.printUninteresting = printUninteresting;
    this.considerJustification = considerJustification;
  }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (shouldPrint(inv1, inv2)) {
      super.visitInvNode(node);
    }
  }

  private boolean shouldPrint(Invariant inv1, Invariant inv2) {
      return
        (invariantsDiffer(inv1, inv2) ||
         (considerJustification && justificationsDiffer(inv1, inv2))) &&
        (interestingDifference(inv1, inv2) || printUninteresting) &&
        (differenceWorthPrinting(inv1, inv2) || !considerJustification);
  }

  private boolean differenceWorthPrinting(Invariant inv1, Invariant inv2) {
    return isJustified(inv1) || isJustified(inv2);
  }

  private boolean isJustified(Invariant inv) {
    return (inv != null && inv.justified());
  }

  private boolean invariantsDiffer(Invariant inv1, Invariant inv2) {
    return
      inv1 == null ||
      inv2 == null ||
      !inv1.isSameInvariant(inv2);
  }

  private boolean justificationsDiffer(Invariant inv1, Invariant inv2) {
    boolean inv1Justified = inv1.justified();
    boolean inv2Justified = inv2.justified();
    return
      (inv1Justified && !inv2Justified) ||
      (!inv1Justified && inv2Justified);
  }

  private static boolean interestingDifference(Invariant inv1,
                                               Invariant inv2) {
    return StatisticsVisitor.interestingDifference(inv1, inv2);
  }

  protected void printInvariant(Invariant inv, InvNode node) {
    super.printInvariant(inv, node);
    if (!verbose) {
      printJustificationMaybe(inv, node);
    }
  }

  private void printJustificationMaybe(Invariant inv, InvNode node) {
    if (considerJustification &&
        ! invariantsDiffer(node.getInv1(), node.getInv2())) {
      print(" {");
      double prob = inv.getProbability();
      String probString = PROBABILITY_FORMAT.format(prob);
      // Print a ~ if the probability is close to, but not exactly, zero
      if (probString.equals("0") && prob != 0) {
        print("~");
      }
      print(probString);
      print("}");
    }
  }
}
