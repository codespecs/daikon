package MapQuick;

import MapQuick2.*;
import java.util.Iterator;
import junit.framework.*;

public class DFAnyTestCase extends TestCase {
  /** @effects creates a new test which runs the given test in the given mode */
  public DFAnyTestCase(String name, TestRecord test) {
    super(name);
    this.test = test;
  }

  /**
   * @requires name = "testLoadDatabase"
   * @effects creates a new test which attemps to load the database
   */
  public DFAnyTestCase(String name) {
    super(name);
    this.test = null;
  }

  protected final TestRecord test;

  // Store the DirectionsDFinder in a static field, so that we only
  // make one DirectionsFinder no matter how many tests we
  // instantiate.  Initialize via thunk so that if the database cannot
  // be loaded, we fail on the first test, instead of on class-loading
  // of the test suite.
  protected static DirectionsFinder df = null; // (df != null) ==> !df_load_failed
  protected static boolean df_load_failed = false; // df_load_failed ==> (df == null)

  // Upon normal return:      (df != null) && (df_load_failed == false)
  // Upon exceptional return: (df == null) && (df_load_failed == true)
  protected static void loadDatabase() {
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
}
