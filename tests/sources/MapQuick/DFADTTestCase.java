package MapQuick;

import java.util.*;

class DFADTTestCase extends DFAnyTestCase {
  public DFADTTestCase(String name, TestRecord test) {
    super(name, test);
  }

  /** Runs all of the tests in this. */
  public void runTest() {
    testProgADT();
  }

  /** Runs the test against getDirections(Address x 2) */
  @Test
  public void testProgADT() {
    loadDatabase();
    String useful_name = "query from '" + test.start + "' to '" + test.end + "'";
    if (test.directions != null) {
      runProgADT_directions(useful_name);
    } else if (test.invalid_address_message != null) {
      runProgADT_bad_address(useful_name);
    } else if (test.no_path_message != null) {
      runProgADT_no_path(useful_name);
    } else {
      throw new IllegalStateException("Unknown desired result");
    }
  }

  private void runProgADT_bad_address(String useful_name) {
    try {
      Directions result = df.getDirections(test.start, test.end);
      fail("Expected InvalidAddressException on " + useful_name);
    } catch (InvalidAddressException e) {
      // this is what we want
      return;
    } catch (NoPathException e) {
      fail("Unexpected exception on " + useful_name + ": " + e.toString());
    }
  }

  private void runProgADT_no_path(String useful_name) {
    try {
      Directions result = df.getDirections(test.start, test.end);
      fail("Expected NoPathException on " + useful_name);
    } catch (InvalidAddressException e) {
      fail("Unexpected exception on " + useful_name + ": " + e.toString());
    } catch (NoPathException e) {
      // this is what we want
      return;
    }
  }

  private void runProgADT_directions(String useful_name) {
    try {
      Directions result = df.getDirections(test.start, test.end);

      assertEquals(
          "The start given by getStart() should match the starting point given in the query ("
              + useful_name
              + ")",
          test.start,
          result.getStart());

      assertEquals(
          "The end given by getEnd() should match the ending point given in the query ("
              + useful_name
              + ")",
          test.end,
          result.getEnd());

      assertEquals(
          "The length given by getLength() should match the expected length for " + useful_name,
          test.length,
          result.getLength(),
          0.001);

      Iterator query_dirs = Arrays.asList(test.directions).iterator();
      Iterator result_dirs = result.getDirections();
      assertEquals("getDirections(Address x 2) " + useful_name, query_dirs, result_dirs);

    } catch (InvalidAddressException e) {
      fail("Unexpected exception on " + useful_name + ": " + e.toString());
    } catch (NoPathException e) {
      fail("Unexpected exception on " + useful_name + ": " + e.toString());
    }
  }
}
