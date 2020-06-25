package MapQuick;

import org.junit.Test;

class DFStringTestCase extends DFAnyTestCase {
  public DFStringTestCase(String name, TestRecord test) {
    super(name, test);
  }

  /** Runs all of the tests in this. */
  public void runTest() {
    testProgString();
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
}
