package daikon.split;

/**
 * MethodParameter is a simple immutable ADT for representing
 * the name and type of a method parameter.
 */
class MethodParameter {

  /** The type of this. */
  private String type;

  /** The name of this. */
  private String name;

  /**
   * Creates a new instance of MethodParameter with the given
   * name and type.
   * @param name the desired name of this.
   * @param type the desired type of this.
   */
  public MethodParameter(String name, String type) {
    this.name = name;
    this.type = type;
  }

  /**
   * Returns the name of this.
   */
  public String getName() {
    return name;
  }

  /**
   * Returns the type of this.
   */
  public String getType() {
    return type;
  }

}
