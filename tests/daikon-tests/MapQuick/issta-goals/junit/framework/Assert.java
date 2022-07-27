package junit.framework;

/**
 * A set of assert methods.
 */
public class Assert {

  /**
   * Protect constructor since it is a static only class
   */
  protected Assert() {
  }

  /**
   * Asserts that a condition is true. If it isn't it throws
   * an AssertionFailedError with the given message.
   */
  //@ensures (condition == true)
  static public void assert(String message, boolean condition) {
    if (!condition)
      fail(message);
  }

  /**
   * Asserts that a condition is true. If it isn't it throws
   * an AssertionFailedError.
   */
  //@ensures (condition == true)
  static public void assert(boolean condition) {
    assert(null, condition);
  }

  /**
   * Fails a test with the given message. 
   */
  static public void fail(String message) {
    throw new AssertionFailedError(message);
  }

  /**
   * Fails a test with no message. 
   */
  static public void fail() {
    fail(null);
  }

  /**
   * Asserts that two objects are equal. If they are not
   * an AssertionFailedError is thrown.
   */
  static public void assertEquals(String message, Object expected, Object actual) {
    if (expected == null && actual == null)
      return;
    if (expected != null && expected.equals(actual))
      return;
    failNotEquals(message, expected, actual);
  }

  /**
   * Asserts that two objects are equal. If they are not
   * an AssertionFailedError is thrown.
   */
  static public void assertEquals(Object expected, Object actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two doubles are equal.
   */
  static public void assertEquals(String message, double expected, double actual, double delta) {
    if (!(Math.abs(expected-actual) <= delta)) // Because comparison with NaN always returns false
      failNotEquals(message, Double.valueOf(expected), Double.valueOf(actual));
  }

  /**
   * Asserts that two doubles are equal.
   */
  static public void assertEquals(double expected, double actual, double delta) {
    assertEquals(null, expected, actual, delta);
  }

  /**
   * Asserts that two floats are equal concerning a delta
   */
  static public void assertEquals(String message, float expected, float actual, float delta) {
    if (!(Math.abs(expected-actual) <= delta))
      failNotEquals(message, new Float(expected), new Float(actual));
  }

  /**
   * Asserts that two floats are equal concerning a delta
   */
  static public void assertEquals(float expected, float actual, float delta) {
    assertEquals(null, expected, actual, delta);
  }

  /**
   * Asserts that two longs are equal.
   */
  static public void assertEquals(String message, long expected, long actual) {
    assertEquals(message, Long.valueOf(expected), Long.valueOf(actual));
  }

  /**
   * Asserts that two longs are equal.
   */
  static public void assertEquals(long expected, long actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two booleans are equal.
   */
  static public void assertEquals(String message, boolean expected, boolean actual) {
    assertEquals(message, Boolean.valueOf(expected), Boolean.valueOf(actual));
  }

  /**
   * Asserts that two booleans are equal.
   */
  static public void assertEquals(boolean expected, boolean actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two bytes are equal.
   */
  static public void assertEquals(String message, byte expected, byte actual) {
    assertEquals(message, new Byte(expected), new Byte(actual));
  }

  /**
   * Asserts that two bytes are equal.
   */
  static public void assertEquals(byte expected, byte actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two chars are equal.
   */
  static public void assertEquals(String message, char expected, char actual) {
    assertEquals(message, Character.valueOf(expected), Character.valueOf(actual));
  }

  /**
   * Asserts that two chars are equal.
   */
  static public void assertEquals(char expected, char actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two shorts are equal.
   */
  static public void assertEquals(String message, short expected, short actual) {
    assertEquals(message, new Short(expected), new Short(actual));
  }

  /**
   * Asserts that two shorts are equal.
   */
  static public void assertEquals(short expected, short actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that two ints are equal.
   */
  static public void assertEquals(String message, int expected, int actual) {
    assertEquals(message, new Integer(expected), new Integer(actual));
  }

  /**
   * Asserts that two ints are equal.
   */
  static public void assertEquals(int expected, int actual) {
    assertEquals(null, expected, actual);
  }

  /**
   * Asserts that an object isn't null.
   */
  //@ensures (object != null)
  static public void assertNotNull(Object object)
  {
    assertNotNull(null, object);
  }

  /**
   * Asserts that an object isn't null.
   */
  //@ensures (object != null)
  static public void assertNotNull(String message, Object object)
  {
    assert(message, object != null); 
  }

  /**
   * Asserts that an object is null.
   */
  static public void assertNull(Object object) {
    assertNull(null, object);
  }

  /**
   * Asserts that an object is null.
   */
  static public void assertNull(String message, Object object) {
    assert(message, object == null); 
  }

  /**
   * Asserts that two objects refer to the same object. If they are not
   * an AssertionFailedError is thrown.
   */
  static public void assertSame(String message, Object expected, Object actual) {
    if (expected == actual)
      return;
    failNotSame(message, expected, actual);
  }

  /**
   * Asserts that two objects refer to the same object. If they are not
   * the same an AssertionFailedError is thrown.
   */
  static public void assertSame(Object expected, Object actual) {
    assertSame(null, expected, actual);
  }

  static private void failNotEquals(String message, Object expected, Object actual) {
    String formatted= "";
    if (message != null)
      formatted= message+" ";
    fail(formatted+"expected:<"+expected+"> but was:<"+actual+">");
  }

  static private void failNotSame(String message, Object expected, Object actual) {
    String formatted= "";
    if (message != null)
      formatted= message+" ";
    fail(formatted+"expected same");
  }
}
