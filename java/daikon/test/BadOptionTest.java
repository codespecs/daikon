package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.DaikonGetopt;
import daikon.PrintInvariants;
import daikon.SplitDtrace;
import daikon.UnionInvariants;
import daikon.tools.DtraceDiff;
import daikon.tools.TraceSelect;
import gnu.getopt.LongOpt;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.After;
import org.junit.Test;
import org.junit.function.ThrowingRunnable;

/**
 * Tests that Daikon tools reject bad command-line options with a UserError.
 *
 * <p>These tests do not call {@code Daikon.mainHelper}, which runs Daikon. However, {@link
 * #testHelpOption} passes Daikon arguments to TraceSelect, which checks them by calling {@code
 * Daikon.cleanup} and {@code Daikon.read_options}. Those calls reset Daikon's global state.
 */
public class BadOptionTest {

  /** The long options used by {@link #parse}. DaikonGetopt adds {@code --help}. */
  private static final LongOpt[] longopts =
      new LongOpt[] {
        new LongOpt("config", LongOpt.REQUIRED_ARGUMENT, null, 0),
        new LongOpt("config_option", LongOpt.REQUIRED_ARGUMENT, null, 0),
        new LongOpt("debug", LongOpt.REQUIRED_ARGUMENT, null, 'd'),
        new LongOpt("verbose", LongOpt.NO_ARGUMENT, null, 'v'),
      };

  /**
   * Resets the Daikon options that the TraceSelect tests set. TraceSelect checks its Daikon
   * arguments by calling {@code Daikon.read_options}, so, for example, a {@code
   * --ppt-select-pattern} argument sets {@link Daikon#ppt_regexp}, which would make later tests in
   * the same JVM omit their program points, and the {@code -o} option that TraceSelect passes to
   * Daikon sets {@link Daikon#inv_file}.
   */
  @After
  public void resetDaikonOptions() {
    Daikon.cleanup();
  }

  /** The suffix of every description of a bad command-line option. */
  private static final String USAGE_HINT = "; run with -h for usage";

  /**
   * Parses the given command-line arguments, which must contain a bad option, and returns the
   * resulting error message, without its suffix that suggests running with -h.
   *
   * @param optstring the short options, in the format of Getopt
   * @param args command-line arguments
   * @return the message of the UserError for the bad option, without {@link #USAGE_HINT}
   */
  private static String parseWith(String optstring, String... args) {
    DaikonGetopt g = new DaikonGetopt(args, optstring, longopts, "usage");
    Daikon.UserError e = assertThrows(Daikon.UserError.class, () -> consumeAll(g));
    String message = String.valueOf(e.getMessage());
    assertTrue(message, message.endsWith(USAGE_HINT));
    return message.substring(0, message.length() - USAGE_HINT.length());
  }

  /**
   * Parses the given command-line arguments, which must contain a bad option, and returns the
   * resulting error message, without its suffix that suggests running with -h.
   *
   * @param args command-line arguments
   * @return the message of the UserError for the bad option, without {@link #USAGE_HINT}
   */
  private static String parse(String... args) {
    return parseWith("vo:", args);
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
    assertEquals("Unrecognized command-line option --bogus", parse("-v", "--bogus", "file"));
  }

  /** Tests an unrecognized short option, alone and combined with a valid one. */
  @Test
  public void testUnrecognizedShortOption() {
    assertEquals("Unrecognized command-line option -x", parse("-x"));
    assertEquals("Unrecognized command-line option -x", parse("-xv"));
    assertEquals("Unrecognized command-line option -x", parse("-vx"));
    assertEquals("Unrecognized command-line option -:", parse("-:"));
  }

  /** Tests an unrecognized short option when there are no short options. */
  @Test
  public void testNoShortOptions() {
    for (String arg : new String[] {"- ", "-x", "-:"}) {
      assertEquals("Unrecognized command-line option " + arg, parseWith("", arg));
    }
  }

  /** Tests an ambiguous abbreviation of a long option. */
  @Test
  public void testAmbiguousLongOption() {
    assertEquals("Ambiguous command-line option --conf", parse("--conf=x"));
    assertEquals("Ambiguous command-line option --conf", parse("--conf"));
  }

  /** Tests options that are missing their required argument. */
  @Test
  public void testMissingArgument() {
    assertEquals("Command-line option --config requires an argument", parse("--config"));
    assertEquals("Command-line option --debug requires an argument", parse("--deb"));
    assertEquals("Command-line option -o requires an argument", parse("-o"));
    assertEquals("Command-line option -o requires an argument", parse("-vo"));
  }

  /** Tests options that are missing their required argument, when the optstring starts with ':'. */
  @Test
  public void testMissingArgumentLeadingColon() {
    assertEquals("Command-line option -o requires an argument", parseWith(":o:", "-o"));
    assertEquals("Command-line option --config requires an argument", parseWith(":o:", "--config"));
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
    assertEquals(description + USAGE_HINT, String.valueOf(e.getMessage()));
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
    assertEquals(
        "Too few arguments; run with -h for usage",
        String.valueOf(
            assertThrows(Daikon.UserError.class, () -> TraceSelect.mainHelper(new String[] {"20"}))
                .getMessage()));
    assertEquals(
        "-SEED requires an argument",
        String.valueOf(
            assertThrows(
                    Daikon.UserError.class,
                    () -> TraceSelect.mainHelper(new String[] {"20", "10", "-SEED"}))
                .getMessage()));
    assertEquals(
        "-SEED requires an integer argument, not abc",
        String.valueOf(
            assertThrows(
                    Daikon.UserError.class,
                    () ->
                        TraceSelect.mainHelper(
                            new String[] {"20", "10", "-SEED", "abc", "x.dtrace"}))
                .getMessage()));
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
    // TraceSelect options must precede the Daikon arguments.
    e =
        assertThrows(
            Daikon.UserError.class,
            () -> TraceSelect.mainHelper(new String[] {"20", "10", "x.dtrace", "-NOCLEAN"}));
    assertEquals(
        "TraceSelect option -NOCLEAN must precede the Daikon arguments, such as the .dtrace file",
        String.valueOf(e.getMessage()));
    // TraceSelect ignores the case of an option.
    e =
        assertThrows(
            Daikon.UserError.class,
            () -> TraceSelect.mainHelper(new String[] {"20", "10", "x.dtrace", "-noclean"}));
    assertEquals(
        "TraceSelect option -noclean must precede the Daikon arguments, such as the .dtrace file",
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
    // TraceSelect passes -o to Daikon itself.
    e =
        assertThrows(
            Daikon.UserError.class,
            () -> TraceSelect.mainHelper(new String[] {"1", "10", "-o", "r.inv", "x.dtrace"}));
    assertEquals(
        "TraceSelect does not accept Daikon's -o option: it writes the invariants of each sample"
            + " to the sample's file name plus \".inv\"",
        String.valueOf(e.getMessage()));
    // An option's value is not the trace file, even if it ends with ".dtrace".
    e =
        assertThrows(
            Daikon.UserError.class,
            () ->
                TraceSelect.mainHelper(
                    new String[] {"20", "10", "--ppt-select-pattern", "x.dtrace"}));
    assertTrue(String.valueOf(e.getMessage()).startsWith("No .dtrace file name specified"));
    e =
        assertThrows(
            Daikon.UserError.class,
            () ->
                TraceSelect.mainHelper(new String[] {"20", "10", "--ppt-select-pattern=x.dtrace"}));
    assertTrue(String.valueOf(e.getMessage()).startsWith("No .dtrace file name specified"));
  }

  /**
   * Tests that TraceSelect reports a trace file that does not exist, before doing any sampling.
   *
   * @throws IOException if there is trouble creating or deleting the temporary directory
   */
  @Test
  public void testTraceSelectMissingTraceFile() throws IOException {
    Path dir = Files.createTempDirectory("BadOptionTest");
    String dtrace = dir.resolve("missing.dtrace").toString();
    PrintStream oldOut = System.out;
    System.setOut(new PrintStream(new ByteArrayOutputStream(), true, UTF_8));
    try {
      Daikon.UserError e =
          assertThrows(
              Daikon.UserError.class,
              () -> TraceSelect.mainHelper(new String[] {"1", "10", dtrace}));
      // Daikon.read_options, which TraceSelect calls to check its Daikon arguments, reports it.
      assertEquals("File " + dtrace + " not found.", String.valueOf(e.getMessage()));
    } finally {
      System.setOut(oldOut);
      Files.delete(dir);
    }
  }

  /** Tests that tools accept the {@code --help} option that their usage messages document. */
  @Test
  public void testHelpOption() {
    assertPrintsUsage(() -> DtraceDiff.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> UnionInvariants.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"--help"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"20", "10", "-h", "x.dtrace"}));
    assertPrintsUsage(() -> TraceSelect.mainHelper(new String[] {"20", "10", "--help"}));
    assertPrintsUsage(
        () -> TraceSelect.mainHelper(new String[] {"20", "10", "-NOCLEAN", "x.dtrace", "--he"}));
    assertPrintsUsage(() -> SplitDtrace.mainHelper(new String[] {"-h", "x"}));
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
