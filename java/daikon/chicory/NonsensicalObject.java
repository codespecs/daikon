package daikon.chicory;

/**
 * A NonsensicalObject is used during data trace output for variables whose value is "nonsensical"
 * to print. For instance, say class A has a field name. If variable x is of type A is null, then we
 * print "null" for x's value. However, we print "nonsensical" for x.name's value.
 */
public final class NonsensicalObject {

  /** The singleton instance of NonsensicalObject. */
  private static final NonsensicalObject instance = new NonsensicalObject();

  /** constructor */
  private NonsensicalObject() {}

  public static NonsensicalObject getInstance() {
    return instance;
  }
}
