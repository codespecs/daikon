package daikon.diff;

import java.util.*;

public abstract class Node {

  private List children = new ArrayList();
  private Object userObject = null;

  public Node() {
  }

  public Node(Object userObject) {
    this.userObject = userObject;
  }
  
  public void add(Node newChild) {
    children.add(newChild);
  }

  public Iterator children() {
    return children.iterator();
  }

  public Object getUserObject() {
    return userObject;
  }

  public abstract void accept(NodeVisitor v);

}
