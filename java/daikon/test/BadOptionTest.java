package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.PrintInvariants;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

/** Tests that Daikon tools reject bad command-line options with a UserError. */
public class BadOptionTest {

  /**
   * Asserts that running {@code mainCall} throws a UserError that names the given option.
   *
   * @param badOption the bad option, which the error message should name
   * @param mainCall a call to a tool's {@code mainHelper} method
   */
  private static void assertBadOption(String badOption, ThrowingRunnable mainCall) {
    Daikon.UserError e = assertThrows(Daikon.UserError.class, mainCall);
    assertEquals(
        "Bad command-line option " + badOption + "; run with -h for usage",
        String.valueOf(e.getMessage()));
  }

  /** Tests an unrecognized long option. */
  @Test
  public void testDaikonUnrecognizedLongOption() {
    assertBadOption("--bogus", () -> Daikon.mainHelper(new String[] {"--bogus"}));
  }

  /** Tests an unrecognized short option that is combined with a valid one. */
  @Test
  public void testDaikonUnrecognizedShortOption() {
    assertBadOption("-x", () -> Daikon.mainHelper(new String[] {"-xh"}));
  }

  /** Tests a long option that is missing its required argument. */
  @Test
  public void testDaikonMissingArgument() {
    assertBadOption("--config", () -> Daikon.mainHelper(new String[] {"--config"}));
  }

  /** Tests an unrecognized long option in a tool other than Daikon. */
  @Test
  public void testPrintInvariantsUnrecognizedOption() {
    assertBadOption(
        "--bogus", () -> PrintInvariants.mainHelper(new String[] {"--bogus", "foo.inv.gz"}));
  }
}
