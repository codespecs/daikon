package daikon.diff;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.Pair;

/**
 * All nodes must subclass this class. The type parameter CONTENT is (half of) the type of the
 * objects stored in this node: they are {@code Pair<CONTENT,CONTENT>}. The type parameter CHILD is
 * the type of the children (and is ignored if there are no children).
 */
public abstract class Node<CONTENT extends @Nullable Object, CHILD> {

  private List<CHILD> children = new ArrayList<CHILD>();
  // Nonsensical for RootNode
  private Pair<CONTENT, CONTENT> userObject;

  public Node(Pair<CONTENT, CONTENT> userObject) {
    this.userObject = userObject;
  }

  public Node(CONTENT left, CONTENT right) {
    this.userObject = Pair.of(left, right);
  }

  public void add(CHILD newChild) {
    children.add(newChild);
  }

  public Iterator<CHILD> children() {
    return children.iterator();
  }

  public Pair<CONTENT, CONTENT> getUserObject() {
    return userObject;
  }

  @Pure
  public CONTENT getUserLeft() {
    return userObject.a;
  }

  @Pure
  public CONTENT getUserRight() {
    return userObject.b;
  }

  public abstract void accept(Visitor v);
}
