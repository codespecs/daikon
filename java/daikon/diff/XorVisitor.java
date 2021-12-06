package daikon.diff;

import daikon.PptTopLevel;
import daikon.inv.Invariant;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.checkerframework.checker.nullness.qual.EnsuresNonNullIf;
import org.checkerframework.checker.nullness.qual.MonotonicNonNull;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;

/** Computes A xor B, where A and B are the two sets of invariants. */
public class XorVisitor extends DepthFirstVisitor {

  private InvMap result = new InvMap();
  private @MonotonicNonNull PptTopLevel currentPpt;

  public static final Logger debug = Logger.getLogger("daikon.diff.XorVisitor");

  /** Every node has at least one non-null ppt. Add one of the non-null ppt to the result. */
  @Override
  public void visit(PptNode node) {
    PptTopLevel ppt1 = node.getPpt1();
    PptTopLevel ppt2 = node.getPpt2();
    @SuppressWarnings(
        "nullness") // application invariant: at least one of ppt1 and ppt2 is non-null
    @NonNull PptTopLevel pptNonNull = (ppt1 != null ? ppt1 : ppt2);
    result.addPpt(pptNonNull);
    currentPpt = pptNonNull;
    super.visit(node);
  }

  /**
   * If one invariant is null and the other is not, add the non-null invariant to the result set.
   */
  @Override
  @SuppressWarnings(
      "nullness:contracts.precondition.override") // visitor invariant, because the PptNode
  // has already been visited
  @RequiresNonNull("currentPpt")
  // visitor invariant
  public void visit(InvNode node) {
    Invariant inv1 = node.getInv1();
    Invariant inv2 = node.getInv2();
    if (debug.isLoggable(Level.FINE)) {
      debug.fine(
          "visit: "
              + ((inv1 != null) ? inv1.ppt.parent.name() : "NULL")
              + " "
              + ((inv1 != null) ? inv1.repr() : "NULL")
              + " - "
              + ((inv2 != null) ? inv2.repr() : "NULL"));
    }
    if (shouldAddInv1(inv1, inv2)) {
      assert inv1 != null;
      result.add(currentPpt, inv1);
    } else if (shouldAddInv2(inv1, inv2)) {
      assert inv2 != null;
      result.add(currentPpt, inv2);
    }
  }

  @EnsuresNonNullIf(result = true, expression = "#1")
  private static boolean shouldAddInv1(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    return (inv1 != null) && (inv2 == null);
  }

  @EnsuresNonNullIf(result = true, expression = "#2")
  private static boolean shouldAddInv2(@Nullable Invariant inv1, @Nullable Invariant inv2) {
    return (inv2 != null) && (inv1 == null);
  }

  /** Returns the InvMap generated as a result of the traversal. */
  public InvMap getResult() {
    return result;
  }
}
