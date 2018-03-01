package daikon.diff;

import java.util.Iterator;

/**
 * Provides default methods which visit each node in the tree in depth-first order. Other visitors
 * may extend this class.
 */
public class DepthFirstVisitor implements Visitor {

  @Override
  public void visit(RootNode node) {
    for (Iterator<PptNode> i = node.children(); i.hasNext(); ) {
      i.next().accept(this);
    }
  }

  @Override
  public void visit(PptNode node) {
    for (Iterator<InvNode> i = node.children(); i.hasNext(); ) {
      i.next().accept(this);
    }
  }

  @Override
  public void visit(InvNode node) {}
}
