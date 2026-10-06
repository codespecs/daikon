package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.DaikonGetopt;
import daikon.PrintInvariants;
import daikon.UnionInvariants;
import daikon.tools.DtraceDiff;
import gnu.getopt.LongOpt;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

/**
 * Tests that Daikon tools reject bad command-line options with a UserError.
 *
 * <p>These tests do not call {@code Daikon.mainHelper}, because it resets Daikon's global state,
 * which other tests in the same JVM depend on.
 */
public class BadOptionTest {

  /** The long options used by {@link #parse}. */
  private static final LongOpt[] longopts =
      new LongOpt[] {
        new LongOpt("help", LongOpt.NO_ARGUMENT, null, 0),
        new LongOpt("config", LongOpt.REQUIRED_ARGUMENT, null, 0),
        new LongOpt("config_option", LongOpt.REQUIRED_ARGUMENT, null, 0),
        new LongOpt("debug", LongOpt.REQUIRED_ARGUMENT, null, 'd'),
        new LongOpt("verbose", LongOpt.NO_ARGUMENT, null, 'v'),
      };

  /**
   * Parses the given command-line arguments, which must contain a bad option, and returns the
   * resulting error message.
   *
   * @param args command-line arguments
   * @return the message of the UserError for the bad option
   */
  private static String parse(String... args) {
    DaikonGetopt g = new DaikonGetopt("BadOptionTest", args, "ho:", longopts);
    int c;
    while ((c = g.getopt()) != -1) {
      if (c == '?') {
        return String.valueOf(g.badOptionError().getMessage());
      }
    }
    throw new AssertionError("no bad option");
  }

  /**
   * Returns the expected message for a bad option.
   *
   * @param description a description of the bad option
   * @return the expected message for a bad option
   */
  private static String expected(String description) {
    return description + "; run with -h for usage";
  }

  /** Tests an unrecognized long option. */
  @Test
  public void testUnrecognizedLongOption() {
    assertEquals(expected("Unrecognized command-line option --bogus"), parse("--bogus"));
    assertEquals(expected("Unrecognized command-line option --bogus"), parse("--bogus=3"));
    assertEquals(
        expected("Unrecognized command-line option --bogus"), parse("-h", "--bogus", "file"));
  }

  /** Tests an unrecognized short option, alone and combined with a valid one. */
  @Test
  public void testUnrecognizedShortOption() {
    assertEquals(expected("Unrecognized command-line option -x"), parse("-x"));
    assertEquals(expected("Unrecognized command-line option -x"), parse("-xh"));
    assertEquals(expected("Unrecognized command-line option -x"), parse("-hx"));
    assertEquals(expected("Unrecognized command-line option -:"), parse("-:"));
  }

  /** Tests an ambiguous abbreviation of a long option. */
  @Test
  public void testAmbiguousLongOption() {
    assertEquals(expected("Ambiguous command-line option --conf"), parse("--conf=x"));
  }

  /** Tests options that are missing their required argument. */
  @Test
  public void testMissingArgument() {
    assertEquals(expected("Command-line option --config requires an argument"), parse("--config"));
    assertEquals(expected("Command-line option --debug requires an argument"), parse("--deb"));
    assertEquals(expected("Command-line option -o requires an argument"), parse("-o"));
    assertEquals(expected("Command-line option -o requires an argument"), parse("-ho"));
  }

  /** Tests long options that are given an argument they do not take. */
  @Test
  public void testUnexpectedArgument() {
    assertEquals(
        expected("Command-line option --help does not take an argument"), parse("--help=x"));
    assertEquals(
        expected("Command-line option --verbose does not take an argument"), parse("--verb=x"));
  }

  /**
   * Asserts that running {@code mainCall} throws a UserError with the given message.
   *
   * @param description a description of the bad option
   * @param mainCall a call to a tool's {@code mainHelper} method
   */
  private static void assertBadOption(String description, ThrowingRunnable mainCall) {
    Daikon.UserError e = assertThrows(Daikon.UserError.class, mainCall);
    assertEquals(expected(description), String.valueOf(e.getMessage()));
  }

  /** Tests an unrecognized long option in a tool. */
  @Test
  public void testPrintInvariantsUnrecognizedOption() {
    assertBadOption(
        "Unrecognized command-line option --bogus",
        () -> PrintInvariants.mainHelper(new String[] {"--bogus", "foo.inv.gz"}));
  }

  /** Tests that tools accept the {@code --help} option that their usage messages document. */
  @Test
  public void testHelpOption() {
    assertThrows(
        Daikon.NormalTermination.class, () -> DtraceDiff.mainHelper(new String[] {"--help"}));
    assertThrows(
        Daikon.NormalTermination.class, () -> UnionInvariants.mainHelper(new String[] {"--help"}));
  }
}
