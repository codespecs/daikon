package daikon.diff;

/**
 * The root of the tree.  All its children are PptNodes.
 **/
public class RootNode extends Node<Void,PptNode> {

  public RootNode() {
    super();
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
