package MapQuick1;

import MapQuick.*;

/**
 * Checked exception to indicate that a path was not found.
 */
public class NoPathException
  extends Exception
{
  public NoPathException ()
  {
    super ();
  }
  public NoPathException(String s)
  {
    super(s);
  }
}
