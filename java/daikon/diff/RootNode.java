package daikon.diff;

import java.util.Iterator;

public class RootNode extends Node {
  
  public RootNode() {
    super();
  }

  public void accept(NodeVisitor v) {
    v.visitRootNode(this);
    for (Iterator i = children(); i.hasNext(); ) {
      Node node = (Node) i.next();
      node.accept(v);
    }
  }
}
