package daikon.simplify;

/** Superclass of all RuntimeExceptions in this package. */
public class SimplifyError extends RuntimeException {
  static final long serialVersionUID = 20020122L;

  public SimplifyError() {
    super();
  }

  public SimplifyError(String s) {
    super(s);
  }

  /**
   * Constructs a new SimplifyError with the specified cause.
   *
   * @param cause the cause
   */
  public SimplifyError(Throwable cause) {
    super(cause);
  }
}
