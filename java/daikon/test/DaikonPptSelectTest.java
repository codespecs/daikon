package daikon.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.PrintInvariants;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests that the {@code --ppt-select-pattern} and {@code --ppt-omit-pattern} options of Daikon
 * apply to the invariants that Daikon prints at a combined exit point, which Daikon creates rather
 * than reading its declaration.
 */
public class DaikonPptSelectTest {

  /** A temporary directory for the input and output files of Daikon. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /**
   * The declarations of a method {@code C.m(int)} that has a single numbered exit point, {@code
   * C.m(int):::EXIT5}. Daikon creates the combined exit point {@code C.m(int):::EXIT}.
   */
  private static final String DECLS =
      String.join(
          System.lineSeparator(),
          "decl-version 2.0",
          "var-comparability none",
          "",
          "ppt C.m(int):::ENTER",
          "ppt-type enter",
          "variable x",
          "  var-kind variable",
          "  dec-type int",
          "  rep-type int",
          "  flags is_param",
          "",
          "ppt C.m(int):::EXIT5",
          "ppt-type subexit",
          "variable x",
          "  var-kind variable",
          "  dec-type int",
          "  rep-type int",
          "  flags is_param",
          "variable return",
          "  var-kind return",
          "  dec-type int",
          "  rep-type int",
          "");

  /** Resets the static fields that Daikon and PrintInvariants set from command-line options. */
  @Before
  @After
  public void cleanup() {
    PrintInvariants.cleanup();
  }

  /**
   * Returns the samples of a data trace file for {@link #DECLS}, in which {@code C.m(int)} is
   * called with arguments 1, 2, and 3 and returns its argument plus 1.
   *
   * @return the contents of a data trace file
   */
  private static String dtrace() {
    StringBuilder sb = new StringBuilder();
    for (int nonce = 0; nonce < 30; nonce++) {
      int x = 1 + nonce % 3;
      sb.append(String.format("%nC.m(int):::ENTER%nthis_invocation_nonce%n%d%n", nonce));
      sb.append(String.format("x%n%d%n1%n", x));
      sb.append(String.format("%nC.m(int):::EXIT5%nthis_invocation_nonce%n%d%n", nonce));
      sb.append(String.format("x%n%d%n1%nreturn%n%d%n1%n", x, x + 1));
    }
    return sb.toString();
  }

  /**
   * Runs Daikon on {@link #DECLS} and {@link #dtrace} and returns the lines that it prints.
   *
   * @param options command-line options for Daikon
   * @return the lines that Daikon prints to standard output
   * @throws IOException if Daikon cannot read or write a file
   */
  private List<String> runDaikon(String... options) throws IOException {
    File declsFile = tmpFolder.newFile("test.decls");
    Files.write(declsFile.toPath(), DECLS.getBytes(StandardCharsets.UTF_8));
    File dtraceFile = tmpFolder.newFile("test.dtrace");
    Files.write(
        dtraceFile.toPath(), ("decl-version 2.0" + dtrace()).getBytes(StandardCharsets.UTF_8));
    File invFile = new File(tmpFolder.getRoot(), "test.inv.gz");

    List<String> args = new ArrayList<>(Arrays.asList(options));
    args.addAll(
        Arrays.asList(
            "--no_show_progress",
            "-o",
            invFile.toString(),
            declsFile.toString(),
            dtraceFile.toString()));

    PrintStream oldOut = System.out;
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();
    try (PrintStream newOut = new PrintStream(bytes, true, StandardCharsets.UTF_8)) {
      System.setOut(newOut);
      Daikon.mainHelper(args.toArray(new String[0]));
    } finally {
      System.setOut(oldOut);
    }
    return Arrays.asList(bytes.toString(StandardCharsets.UTF_8).split("\\R"));
  }

  /** Without a pattern, Daikon prints the combined exit point and not the numbered exit point. */
  @Test
  public void testNoPattern() throws IOException {
    List<String> lines = runDaikon();
    assertTrue(lines.toString(), lines.contains("C.m(int):::EXIT"));
    assertFalse(lines.toString(), lines.contains("C.m(int):::EXIT5"));
  }

  /**
   * If Daikon's {@code --ppt-select-pattern} matches the numbered exit point and not the combined
   * exit point, then Daikon prints the numbered exit point and not the combined exit point.
   */
  @Test
  public void testSelectNumberedExit() throws IOException {
    List<String> lines = runDaikon("--ppt-select-pattern=ENTER|EXIT[0-9]");
    assertTrue(lines.toString(), lines.contains("C.m(int):::EXIT5"));
    assertTrue(lines.toString(), lines.contains("return > orig(x)"));
    assertFalse(lines.toString(), lines.contains("C.m(int):::EXIT"));
  }

  /**
   * If Daikon's {@code --ppt-omit-pattern} matches the combined exit point and not the numbered
   * exit point, then Daikon prints the numbered exit point and not the combined exit point.
   */
  @Test
  public void testOmitCombinedExit() throws IOException {
    List<String> lines = runDaikon("--ppt-omit-pattern=:::EXIT$");
    assertTrue(lines.toString(), lines.contains("C.m(int):::EXIT5"));
    assertTrue(lines.toString(), lines.contains("return > orig(x)"));
    assertFalse(lines.toString(), lines.contains("C.m(int):::EXIT"));
  }
}
