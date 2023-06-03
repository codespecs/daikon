package daikon.diff;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.IPair;

/**
 * All nodes must subclass this class. The type parameter CONTENT is (half of) the type of the
 * objects stored in this node: they are {@code IPair<CONTENT,CONTENT>}. The type parameter CHILD is
 * the type of the children (and is ignored if there are no children).
 */
public abstract class Node<CONTENT extends @Nullable Object, CHILD> {

  private List<CHILD> children = new ArrayList<>();
  // Nonsensical for RootNode
  private IPair<CONTENT, CONTENT> userObject;

  protected Node(IPair<CONTENT, CONTENT> userObject) {
    this.userObject = userObject;
  }

  protected Node(CONTENT left, CONTENT right) {
    this.userObject = IPair.of(left, right);
  }

  public void add(CHILD newChild) {
    children.add(newChild);
  }

  public Iterator<CHILD> children() {
    return children.iterator();
  }

  public IPair<CONTENT, CONTENT> getUserObject() {
    return userObject;
  }

  @Pure
  public CONTENT getUserLeft() {
    return userObject.first;
  }

  @Pure
  public CONTENT getUserRight() {
    return userObject.second;
  }

  public abstract void accept(Visitor v);
}
