package daikon.diff;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.dataflow.qual.Pure;
import org.plumelib.util.IPair;

/**
 * All nodes must subclass this class.
 *
 * @param <CONTENT> half of the type of the objects stored in this node, which are {@code
 *     IPair<CONTENT,CONTENT>}
 * @param <CHILD> the type of the children; it is is ignored if there are no children
 */
public abstract class Node<CONTENT extends @Nullable Object, CHILD> {

  /** The children of this node. */
  private List<CHILD> children = new ArrayList<>();

  /** Nonsensical for RootNode. */
  private IPair<CONTENT, CONTENT> userObject;

  /**
   * Creates a new Node.
   *
   * @param userObject the user object
   */
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

  /**
   * Returns the user object pair.
   *
   * @return the user object pair
   */
  public IPair<CONTENT, CONTENT> getUserObject() {
    return userObject;
  }

  /**
   * Returns the first element of the user object pair.
   *
   * @return the first element of the user object pair
   */
  @Pure
  public CONTENT getUserLeft() {
    return userObject.first;
  }

  /**
   * Returns the second element of the user object pair.
   *
   * @return the second element of the user object pair
   */
  @Pure
  public CONTENT getUserRight() {
    return userObject.second;
  }

  public abstract void accept(Visitor v);
}
