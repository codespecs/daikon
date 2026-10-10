package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.DaikonGetopt;
import daikon.PrintInvariants;
import daikon.SplitDtrace;
import daikon.UnionInvariants;
import daikon.tools.DtraceDiff;
import daikon.tools.TraceSelect;
import gnu.getopt.LongOpt;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

/**
 * Tests that Daikon tools reject bad command-line options with a UserError.
 *
 * <p>These tests do not call {@code Daikon.mainHelper} or {@code Daikon.cleanup}, because they
 * reset Daikon's global state, which other tests in the same JVM depend on. TraceSelect checks its
 * Daikon arguments without calling either of them.
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
    DaikonGetopt g = new DaikonGetopt(args, "ho:", longopts);
    Daikon.UserError e = assertThrows(Daikon.UserError.class, () -> consumeAll(g));
    return String.valueOf(e.getMessage());
  }

  /**
   * Calls {@code getopt()} until all command-line arguments have been processed.
   *
   * @param g the command-line parser
   */
  private static void consumeAll(DaikonGetopt g) {
    while (g.getopt() != -1) {}
  }

  /** Tests an unrecognized long option. */
  @Test
  public void testUnrecognizedLongOption() {
    assertEquals("Unrecognized command-line option --bogus", parse("--bogus"));
    assertEquals("Unrecognized command-line option --bogus", parse("--bogus=3"));
    assertEquals("Unrecognized command-line option --=3", parse("--=3"));
    assertEquals("Unrecognized command-line option --bogus", parse("-h", "--bogus", "file"));
  }

  /** Tests an unrecognized short option, alone and combined with a valid one. */
  @Test
  public void testUnrecognizedShortOption() {
    assertEquals("Unrecognized command-line option -x", parse("-x"));
    assertEquals("Unrecognized command-line option -x", parse("-xh"));
    assertEquals("Unrecognized command-line option -x", parse("-hx"));
    assertEquals("Unrecognized command-line option -:", parse("-:"));
  }

  /** Tests an unrecognized short option when there are no short options. */
  @Test
  public void testNoShortOptions() {
    for (String arg : new String[] {"- ", "-x", "-:"}) {
      DaikonGetopt g = new DaikonGetopt(new String[] {arg}, "", longopts);
      Daikon.UserError e = assertThrows(Daikon.UserError.class, g::getopt);
      assertEquals("Unrecognized command-line option " + arg, String.valueOf(e.getMessage()));
    }
  }

  /** Tests an ambiguous abbreviation of a long option. */
  @Test
  public void testAmbiguousLongOption() {
    assertEquals("Ambiguous command-line option --conf", parse("--conf=x"));
    assertEquals("Ambiguous command-line option --conf", parse("--conf"));
  }

  /** Tests that the description of a bad option suggests -h if DaikonGetopt handles -h. */
  @Test
  public void testUsageHint() {
    DaikonGetopt g = new DaikonGetopt(new String[] {"--bogus"}, "o:", new LongOpt[0], "usage");
    Daikon.UserError e = assertThrows(Daikon.UserError.class, g::getopt);
    assertEquals(
        "Unrecognized command-line option --bogus; run with -h for usage",
        String.valueOf(e.getMessage()));
  }

  /** Tests options that are missing their required argument. */
  @Test
  public void testMissingArgument() {
    assertEquals("Command-line option --config requires an argument", parse("--config"));
    assertEquals("Command-line option --debug requires an argument", parse("--deb"));
    assertEquals("Command-line option -o requires an argument", parse("-o"));
    assertEquals("Command-line option -o requires an argument", parse("-ho"));
  }

  /** Tests options that are missing their required argument, when the optstring starts with ':'. */
  @Test
  public void testMissingArgumentLeadingColon() {
    DaikonGetopt g = new DaikonGetopt(new String[] {"-o"}, ":ho:", longopts);
    Daikon.UserError e = assertThrows(Daikon.UserError.class, g::getopt);
    assertEquals("Command-line option -o requires an argument", String.valueOf(e.getMessage()));
    g = new DaikonGetopt(new String[] {"--config"}, ":ho:", longopts);
    e = assertThrows(Daikon.UserError.class, g::getopt);
    assertEquals(
        "Command-line option --config requires an argument", String.valueOf(e.getMessage()));
  }

  /** Tests long options that are given an argument they do not take. */
  @Test
  public void testUnexpectedArgument() {
    assertEquals("Command-line option --help does not take an argument", parse("--help=x"));
    assertEquals("Command-line option --verbose does not take an argument", parse("--verb=x"));
  }

  /**
   * Asserts that running {@code mainCall} throws a UserError with the given message.
   *
   * @param description a description of the bad option
   * @param mainCall a call to a tool's {@code mainHelper} method
   */
  private static void assertBadOption(String description, ThrowingRunnable mainCall) {
    Daikon.UserError e = assertThrows(Daikon.UserError.class, mainCall);
    assertEquals(description + "; run with -h for usage", String.valueOf(e.getMessage()));
  }

  /** Tests an unrecognized long option in a tool. */
  @Test
  public void testPrintInvariantsUnrecognizedOption() {
    assertBadOption(
        "Unrecognized command-line option --bogus",
        () -> PrintInvariants.mainHelper(new String[] {"--bogus", "foo.inv.gz"}));
  }

  /** Tests that TraceSelect rejects bad arguments before doing any sampling. */
  @Test
  public void testTraceSelectBadArguments() {
    assertBadOption(
        "num_reps must be a positive integer, not 0",
        () -> TraceSelect.mainHelper(new String[] {"0", "10", "x.dtrace"}));
    assertBadOption(
        "num_reps must be a positive integer, not -5",
        () -> TraceSelect.mainHelper(new String[] {"-5", "10", "x.dtrace"}));
    assertBadOption(
        "sample_size must be a positive integer, not -5",
        () -> TraceSelect.mainHelper(new String[] {"20", "-5", "x.dtrace"}));
    assertBadOption(
        "sample_size must be a positive integer, not ten",
        () -> TraceSelect.mainHelper(new String[] {"20", "ten", "x.dtrace"}));
    Daikon.UserError e =
        assertThrows(
            Daikon.UserError.class,
            () -> TraceSelect.mainHelper(new String[] {"20", "10", "-NOCLAEN", "x.dtrace"}));
    assertEquals(
        "Unrecognized TraceSelect option -NOCLAEN; the options are -SEED, -NOCLEAN,"
            + " -INCLUDE_UNRETURNED, and -DO_DIFFS",
        String.valueOf(e.getMessage()));
    // Daikon would read a compressed trace in full, in every sample run.
    e =
        assertThrows(
            Daikon.UserError.class,
            () -> TraceSelect.mainHelper(new String[] {"20", "10", "x.dtrace", "y.dtrace.gz"}));
    assertEquals(
        "TraceSelect cannot sample y.dtrace.gz; the trace file must be uncompressed and its name"
            + " must end with \".dtrace\"",
        String.valueOf(e.getMessage()));
  }

  /** Tests that tools accept the {@code --help} option that their usage messages document. */
  @Test
  public void testHelpOption() {
    assertPrintsUsage(() -> DtraceDiff.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> UnionInvariants.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> SplitDtrace.mainHelper(new String[] {"-h", "x"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"20", "10", "-h", "x.dtrace"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"20", "10", "--help"}));
    assertPrintsUsage(
        () -> TraceSelect.mainHelper(new String[] {"20", "10", "-NOCLEAN", "x.dtrace", "--he"}));
  }

  /**
   * Asserts that the given code prints a usage message to standard output and throws {@link
   * Daikon.NormalTermination}. Captures the usage message so that it does not clutter the test
   * output.
   *
   * @param code the code to run
   */
  private static void assertPrintsUsage(ThrowingRunnable code) {
    PrintStream oldOut = System.out;
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    System.setOut(new PrintStream(bytes, true, UTF_8));
    try {
      assertThrows(Daikon.NormalTermination.class, code);
    } finally {
      System.setOut(oldOut);
    }
    assertNotEquals("", bytes.toString(UTF_8).trim());
  }
}
