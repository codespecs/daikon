package MapQuick;

import junit.framework.Assert;

/**
 * A test record is a fairly simple record type to hold a query and its expected results
 */
public final class TestRecord
{
  /** original of the query; non-null */
  public final Address start;
  /** destination of the query; non-null */
  public final Address end;

  /** sequence of directions lines expected from getDirections, or
      null if we expect not to find any directions.  the elements must
      not contain newlines at the end.  */
  public final String[] directions;
  /** expected result of getLength(), or NaN if we expect not to find
      and directions */
  public final double length;

  /** message expected indicating that an address was invalid, or
      null if the address should be valid */
  public final String invalid_address_message;

  /** message expected indicating that no path could be found or null
      if the addresses are bad or a path should be found */
  public final String no_path_message;

  private TestRecord(Address start,
		     Address end,
		     String[] directions,
		     double length,
		     String invalid_address_message,
		     String no_path_message)
  {
    this.start = start;
    this.end = end;
    this.directions = directions;
    this.length = length;
    this.invalid_address_message = invalid_address_message;
    this.no_path_message = no_path_message;
  }

  /** @return a new TestRecord representing a successful query with the
      given properties */
  public static TestRecord makeDirections(Address start, Address end, String[] directions, double length)
  {
    Assert.assertNotNull(start);
    Assert.assertNotNull(end);
    Assert.assertNotNull(directions);
    Assert.assertTrue(length >= 0);
    return new TestRecord(start, end, directions, length, null, null);
  }

  /** @return a new TestRecord representing an expected bad address
      result */
  public static TestRecord makeBadAddress(Address start, Address end, String message)
  {
    Assert.assertNotNull(start);
    Assert.assertNotNull(end);
    Assert.assertNotNull(message);
    return new TestRecord(start, end, null, Double.NaN, message, null);
  }

  /** @return a new TestRecord representing an expected no path found
      result */
  public static TestRecord makeNoPath(Address start, Address end, String message)
  {
    Assert.assertNotNull(start);
    Assert.assertNotNull(end);
    return new TestRecord(start, end, null, Double.NaN, null, message);
  }

}
