package daikon.diff;

import daikon.PptTopLevel;
import utilMDE.*;

/**
 * Contains a pair of Ppts.  Resides in the second level of the tree.
 * All its children are InvNodes.
 **/
public class PptNode extends Node {

  /** Either ppt1 or ppt2 may be null, but not both. **/
  public PptNode(PptTopLevel ppt1, PptTopLevel ppt2) {
    super(new Pair(ppt1, ppt2));
    Assert.assert(!(ppt1 == null && ppt2 == null),
                  "Both program points may not be null");
  }

  public PptTopLevel getPpt1() {
    Pair p = (Pair) getUserObject();
    return (PptTopLevel) p.a;
  }

  public PptTopLevel getPpt2() {
    Pair p = (Pair) getUserObject();
    return (PptTopLevel) p.b;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
