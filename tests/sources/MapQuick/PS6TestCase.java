package MapQuick;

import MapQuick2.*;
import java.util.Arrays;
import java.util.Iterator;
import junit.framework.*;

public class PS6TestCase extends TestCase {
  /** indicates that the getDirections(Address x 2) interface should be tested */
  public static final int MODE_PROGRAMMATIC_ADT = 0;

  /** indicates that the getDirections(String x 6) interface should be tested */
  public static final int MODE_PROGRAMMATIC_STRING = 1;

  /** indicates that the TextUI interface should be tested */
  public static final int MODE_TEXTUI = 2;

  private static final String[] modeToTest =
      new String[] {
        "testProgADT", "testProgString", "testTextUI",
      };

  /**
   * @requires mode is one of the MODE_* constants defined in by this class
   * @effects creates a new test which runs the given test in the given mode
   */
  public PS6TestCase(TestRecord test, int mode) {
    super(modeToTest[mode]);
    this.test = test;
  }

  /**
   * @requires name = "testLoadDatabase"
   * @effects creates a new test which attemps to load the database
   */
  public PS6TestCase(String name) {
    super(name);
    this.test = null;
  }

  private final TestRecord test;

  // Store the DirectionsDFinder in a static field, so that we only
  // make one DirectionsFinder no matter how many tests we
  // instantiate.  Initialize via thunk so that if the database cannot
  // be loaded, we fail on the first test, instead of on class-loading
  // of the test suite.
  private static DirectionsFinder df = null; // (df != null) ==> !df_load_failed
  private static boolean df_load_failed = false; // df_load_failed ==> (df == null)

  // Upon normal return:      (df != null) && (df_load_failed == false)
  // Upon exceptional return: (df == null) && (df_load_failed == true)
  private static void loadDatabase() {
    // might be done already...
    if (df != null) {
      return;
    }

    // might have failed already ...
    if (df_load_failed) {
      fail("A previous attempt at loading the database has already failed");
    }

    // otherwise, give it a shot...
    try {
      // assume failure
      df_load_failed = true;
      df = DirectionsFinder.getDirectionsFinder(TextUITest.tinyPath, null);
      df_load_failed = false;
    } catch (InvalidDatabaseException e) {
      fail("Load of tiny database failed with an exception: " + e);
    }
  }

  /** Attempts to load the database */
  @Test
  public void testLoadDatabase() {
    loadDatabase();
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

  // (destructively) checks that two iterators return equal elements
  protected static void assertEquals(String message, Iterator expected, Iterator actual) {
    boolean expected_hasnext = expected.hasNext();
    boolean actual_hasnext = actual.hasNext();

    // if both iterators are exhausted, we're fine
    if (!expected_hasnext && !actual_hasnext) {
      return;
    }

    // if just one is exhausted, we fail
    if (expected_hasnext && !actual_hasnext) {
      Object expected_next = expected.next();
      fail(message + ": expected had '" + expected_next + "' but actual had no more elements");
    }

    // if just one is exhausted, we fail
    if (!expected_hasnext && actual_hasnext) {
      Object actual_next = actual.next();
      fail(message + ": actual had '" + actual_next + "' but expected had no more elements");
    }

    // assert that the current element pairing is equal
    Object expected_next = expected.next();
    Object actual_next = actual.next();
    assertEquals(message, expected_next, actual_next);

    // check the rest recursively
    assertEquals(message, expected, actual);
  }

  /** Runs the test against getDirections(String x 6) */
  @Test
  public void testProgString() {
    loadDatabase();
    String useful_name = "query from '" + test.start + "' to '" + test.end + "'";
    if (test.directions != null) {
      runProgString_directions(useful_name);
    } else if (test.invalid_address_message != null) {
      runProgString_bad_address(useful_name);
    } else if (test.no_path_message != null) {
      runProgString_no_path(useful_name);
    } else {
      throw new IllegalStateException("Unknown desired result");
    }
  }

  // @return the result of calling the String x 6 flavor of the
  // programmatic interface on this.test
  private String callString6() {
    return df.getDirections(
        "" + test.start.getNum(),
        test.start.getName(),
        test.start.getZipcode(),
        "" + test.end.getNum(),
        test.end.getName(),
        test.end.getZipcode());
  }

  private void runProgString_directions(String useful_name) {
    String actual = callString6();

    String expected = "Start at " + test.start + "\n";
    for (int i = 0; i < test.directions.length; i++) {
      expected += test.directions[i] + "\n";
    }
    expected +=
        "Trip length: " + (new java.text.DecimalFormat("0.0")).format(test.length) + " miles\n";

    assertEquals(useful_name + ": getDirection(String x 6) output", expected, actual);
  }

  private void runProgString_bad_address(String useful_name) {
    String actual = callString6();
    String expected = test.invalid_address_message + "\n";

    assertEquals(useful_name + ": getDirection(String x 6) output", expected, actual);
  }

  private void runProgString_no_path(String useful_name) {
    String actual = callString6();
    String expected = test.no_path_message + "\n";

    assertEquals(useful_name + ": getDirection(String x 6) output", expected, actual);
  }

  /** Runs the test against the TextUI */
  @Test
  public void testTextUI() {
    // TODO ...
    fail("Unimplemented");
  }
}
