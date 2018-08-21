// PrintNullDiffVisitor.java

package daikon.diff;

import daikon.inv.Invariant;
import java.io.PrintStream;

/**
 * <B>PrintNullDiffVIsitor</B> is a NodeVisitor that only reports an invariant as different when its
 * existence in one set is not in another set. This avoids reported differences simply in confidence
 * changes and other extra-sensitive reports.
 */
public class PrintNullDiffVisitor extends PrintDifferingInvariantsVisitor {

  /** Create an instance of PrintNullDiffVisitor. */
  public PrintNullDiffVisitor(PrintStream ps, boolean verbose) {
    super(ps, verbose, false);
  }

  @Override
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    // If (inv1 XOR inv2) is null
    if (inv1 != null ^ inv2 == null) {
      super.visit(node);
    }
  }
}
