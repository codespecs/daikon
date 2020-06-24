package junit.runner;
/**
 * A listener interface for observing the execution of a test run. Unlike TestListener, this
 * interface using only primitive objects, making it suitable for remote test execution.
 */
public interface TestRunListener {
  /* test status constants*/
  public static final int STATUS_ERROR = 1;
  public static final int STATUS_FAILURE = 2;

  @Test
  public void testRunStarted(String testSuiteName, int testCount);

  @Test
  public void testRunEnded(long elapsedTime);

  @Test
  public void testRunStopped(long elapsedTime);

  @Test
  public void testStarted(String testName);

  @Test
  public void testEnded(String testName);

  @Test
  public void testFailed(int status, String testName, String trace);
}
