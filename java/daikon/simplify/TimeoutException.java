package daikon.simplify;

/**
 * Indicates a request timed out.
 **/
public class TimeoutException
  extends SimplifyException
{
  public TimeoutException() { super(); }
  public TimeoutException(String s) { super(s); }
}
  
