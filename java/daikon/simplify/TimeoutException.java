package daikon.simplify;

/** Indicates a request timed out. */
public class TimeoutException extends SimplifyException {
  static final long serialVersionUID = 20020122L;

  public TimeoutException() {
    super();
  }

  public TimeoutException(String s) {
    super(s);
  }
}
