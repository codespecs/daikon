package daikon.diff;

import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.IPair;

/** The root of the tree. All its children are PptNodes. */
public class RootNode extends Node<Void, PptNode> {

  /** Creates a new RootNode object. */
  @SuppressWarnings({"rawtypes", "unchecked"})
  public RootNode() {
    super((IPair<Void, Void>) (IPair) IPair.of(new Object(), new Object()));
  }

  @Override
  public IPair<Void, Void> getUserObject() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  @Pure
  @Override
  public Void getUserLeft() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  @Pure
  @Override
  public Void getUserRight() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  @Override
  public void accept(Visitor v) {
    v.visit(this);
  }
}
