package daikon.diff;

import daikon.inv.Invariant;
import utilMDE.*;

/**
 * Contains a pair of Invariants.  Resides in the third level of the
 * tree.
 **/
public class InvNode extends Node {

  /** Either inv1 or inv2 may be null, but not both. **/
  public InvNode(Invariant inv1, Invariant inv2) {
    super(new Pair(inv1, inv2));
    Assert.assertTrue(!(inv1 == null && inv2 == null),
                  "Both invariants may not be null");
  }

  public Invariant getInv1() {
    Pair p = (Pair) getUserObject();
    return (Invariant) p.a;
  }

  public Invariant getInv2() {
    Pair p = (Pair) getUserObject();
    return (Invariant) p.b;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
