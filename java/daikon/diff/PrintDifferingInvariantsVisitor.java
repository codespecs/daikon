package daikon.diff;

import daikon.inv.Invariant;
import java.io.*;

public class PrintDifferingInvariantsVisitor extends PrintAllVisitor {

  public PrintDifferingInvariantsVisitor(PrintStream ps,
                                         boolean verbose) {
    super(ps, verbose);
  }

  public void visitInvNode(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (shouldPrint(inv1, inv2)) {
      super.visitInvNode(node);
    }
  }

  private boolean shouldPrint(Invariant inv1, Invariant inv2) {
    boolean shouldPrint = true;

    int type = DetailedStatisticsVisitor.determineType(inv1, inv2);
    if (type == DetailedStatisticsVisitor.TYPE_NULLARY_UNINTERESTING ||
        type == DetailedStatisticsVisitor.TYPE_UNARY_UNINTERESTING) {
      shouldPrint = false;
    }

    int rel = DetailedStatisticsVisitor.determineRelationship(inv1, inv2);
    if (rel == DetailedStatisticsVisitor.REL_SAME_JUST1_JUST2 ||
        rel == DetailedStatisticsVisitor.REL_SAME_UNJUST1_UNJUST2 ||
        rel == DetailedStatisticsVisitor.REL_DIFF_UNJUST1_UNJUST2 ||
        rel == DetailedStatisticsVisitor.REL_MISS_UNJUST1 ||
        rel == DetailedStatisticsVisitor.REL_MISS_UNJUST2) {
      shouldPrint = false;
    }

    return shouldPrint;
  }
}
