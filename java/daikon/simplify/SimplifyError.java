package daikon.simplify;

/**
 * Superclass of all runtime errors in this package.
 **/
public class SimplifyError
  extends RuntimeException
{
  public SimplifyError() { super(); }
  public SimplifyError(String s) { super(s); }
}
  
