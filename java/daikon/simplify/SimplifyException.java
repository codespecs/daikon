package daikon.simplify;

/** Superclass of all checked exceptions in this package. */
public class SimplifyException extends Exception {
  static final long serialVersionUID = 20020122L;

  public SimplifyException() {
    super();
  }

  public SimplifyException(String s) {
    super(s);
  }
}
