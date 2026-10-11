package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.PrintInvariants;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Tests that {@link Daikon#mainHelper} and {@link PrintInvariants#mainHelper} can be called more
 * than once in a JVM, without one call's command-line options affecting a later call.
 */
public class PrintInvariantsRepeatTest {

  /** A directory for the input and output files. */
  @ClassRule public static TemporaryFolder tmp = new TemporaryFolder();

  /** The serialized invariants that Daikon produces from the test input. */
  private static String invFile;

  /** Run Daikon (twice) to create the .inv.gz file. */
  @BeforeClass
  public static void setUpClass() throws IOException {
    File dtrace = new File(tmp.getRoot(), "Hanoi.dtrace.gz");
    try (InputStream in =
        ClassLoader.getSystemResourceAsStream("daikon/test/dtracediff/Hanoi.dtrace.gz")) {
      if (in == null) {
        throw new Error("Cannot find Hanoi.dtrace.gz");
      }
      Files.copy(in, dtrace.toPath());
    }
    invFile = new File(tmp.getRoot(), "Hanoi.inv.gz").toString();
    String[] daikonArgs = {
      "--" + Daikon.no_text_output_SWITCH,
      "--" + Daikon.no_show_progress_SWITCH,
      "-o",
      invFile,
      dtrace.toString()
    };
    Daikon.mainHelper(daikonArgs);
    Daikon.mainHelper(daikonArgs);
  }

  /**
   * Runs PrintInvariants with the given options on {@link #invFile}, and returns its output.
   *
   * @param options command-line options, not including {@code --output} or the input file
   * @return the output of PrintInvariants
   */
  private static String printInvariants(String... options) throws Exception {
    File output = tmp.newFile();
    String[] args = new String[options.length + 3];
    System.arraycopy(options, 0, args, 0, options.length);
    args[options.length] = "--output";
    args[options.length + 1] = output.toString();
    args[options.length + 2] = invFile;
    PrintInvariants.mainHelper(args);
    return new String(Files.readAllBytes(output.toPath()), StandardCharsets.UTF_8);
  }

  @Test
  public void testRepeatedCalls() throws Exception {
    String all = printInvariants();
    assertTrue(all, all.contains("moveDisk"));
    assertTrue(all, all.contains("moveTower"));

    // A --ppt-select-pattern must neither cause an error nor filter output in a later call.
    String moveDisk = printInvariants("--" + Daikon.ppt_regexp_SWITCH, "moveDisk");
    assertTrue(moveDisk, moveDisk.contains("moveDisk"));
    assertFalse(moveDisk, moveDisk.contains("moveTower"));
    assertEquals(moveDisk, printInvariants("--" + Daikon.ppt_regexp_SWITCH, "moveDisk"));

    // --wrap_xml from one call must not affect later calls.
    String xml = printInvariants("--" + Daikon.wrap_xml_SWITCH);
    assertNotEquals(all, xml);

    // Repeating a call, after other calls, produces the same output.
    assertEquals(all, printInvariants());
  }

  @Test
  public void testMultipleOutputOptions() throws Exception {
    String out1 = tmp.newFile().toString();
    String out2 = tmp.newFile().toString();
    assertThrows(
        Daikon.UserError.class,
        () ->
            PrintInvariants.mainHelper(new String[] {"--output", out1, "--output", out2, invFile}));
    // A failed call does not affect a later one.
    assertTrue(printInvariants().contains("moveDisk"));
  }
}
