package daikon.dcomp;

/** Indicates an unexpected error during DynComp processing. */
public class DynCompError extends RuntimeException {

  private static final long serialVersionUID = 1L;

  /**
   * Create a {@link DynCompError} with the given message.
   *
   * @param message the exception message
   */
  public DynCompError(String message) {
    super(message);
  }

  /**
   * Create a {@link DynCompError} with the given message and cause.
   *
   * @param message exception message
   * @param cause exception cause
   */
  public DynCompError(String message, Throwable cause) {
    super(message, cause);
  }
}
