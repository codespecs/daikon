package daikon.diff;

import plume.Pair;

/*>>>
import org.checkerframework.dataflow.qual.*;
*/

/** The root of the tree. All its children are PptNodes. */
public class RootNode extends Node<Void, PptNode> {

  @SuppressWarnings({"rawtypes", "unchecked"})
  public RootNode() {
    super((Pair<Void, Void>) (Pair) Pair.of(new Object(), new Object()));
  }

  public Pair<Void, Void> getUserObject() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  /*@Pure*/
  public Void getUserLeft() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  /*@Pure*/
  public Void getUserRight() {
    throw new Error("Shouldn't ask for userObject for RootNode");
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
