package daikon.diff;

import daikon.inv.Invariant;
import plume.*;

/*>>>
import org.checkerframework.checker.nullness.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/** Contains a pair of Invariants. Resides in the third level of the tree. Has no children. */
public class InvNode extends Node</*@Nullable*/ Invariant, /*@NonNull*/ Void> {

  /** Either inv1 or inv2 may be null, but not both. */
  public InvNode(/*@Nullable*/ Invariant inv1, /*@Nullable*/ Invariant inv2) {
    super(Pair.of(inv1, inv2));
    assert !(inv1 == null && inv2 == null) : "Both invariants may not be null";
  }

  /*@Pure*/
  public /*@Nullable*/ Invariant getInv1() {
    return getUserLeft();
  }

  /*@Pure*/
  public /*@Nullable*/ Invariant getInv2() {
    return getUserRight();
  }

  @Override
  public void accept(Visitor v) {
    v.visit(this);
  }
}
