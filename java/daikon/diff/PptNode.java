package daikon.diff;

import daikon.PptTopLevel;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.IPair;

/**
 * Contains a pair of Ppts. Resides in the second level of the tree. All its children are InvNodes.
 */
public class PptNode extends Node<@Nullable PptTopLevel, InvNode> {

  /**
   * Either ppt1 or ppt2 may be null, but not both.
   *
   * @param ppt1 a program point
   * @param ppt2 a program point
   */
  public PptNode(@Nullable PptTopLevel ppt1, @Nullable PptTopLevel ppt2) {
    super(IPair.of(ppt1, ppt2));
    assert !(ppt1 == null && ppt2 == null) : "Both program points may not be null";
  }

  @Pure
  public @Nullable PptTopLevel getPpt1() {
    return getUserLeft();
  }

  @Pure
  public @Nullable PptTopLevel getPpt2() {
    return getUserRight();
  }

  @Override
  public void accept(Visitor v) {
    v.visit(this);
  }
}
