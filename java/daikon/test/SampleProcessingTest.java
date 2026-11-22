package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests how Daikon processes the samples in a trace file. */
public class SampleProcessingTest {

  /** A temporary directory for decls, dtrace, and inv files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /** Creates a SampleProcessingTest. */
  public SampleProcessingTest() {}

  /**
   * Daikon infers invariants from a sample at a point that is named like an enter point, even
   * though the sample is also recorded as an entry that no exit matches.
   */
  @Test
  public void testPointNamedEnterSample() throws IOException {
    File declsFile = tmpFolder.newFile("point.decls");
    String decls =
        String.join(
            "\n",
            "decl-version 2.0",
            "var-comparability none",
            "",
            "ppt Foo.bar():::ENTER",
            "ppt-type point",
            "variable x",
            "  var-kind variable",
            "  dec-type int",
            "  rep-type int",
            "",
            "");
    Files.write(declsFile.toPath(), decls.getBytes(StandardCharsets.UTF_8));
    File dtraceFile = tmpFolder.newFile("point.dtrace");
    String dtrace = "Foo.bar():::ENTER\nx\n1\n1\n\n";
    Files.write(dtraceFile.toPath(), dtrace.getBytes(StandardCharsets.UTF_8));
    File invFile = new File(tmpFolder.getRoot(), "point.inv.gz");
    try {
      // If Daikon did not count the sample as used, this would throw a UserError: "No samples
      // found for any of 1 program point".
      Daikon.mainHelper(
          new String[] {
            "--no_text_output",
            "-o",
            invFile.toString(),
            declsFile.toString(),
            dtraceFile.toString()
          });
      assertEquals(1, FileIO.samples_used);
      assertTrue(invFile.exists());
    } finally {
      // Also discards the unmatched entry that this test leaves on the call stack.
      Daikon.cleanup();
    }
  }
}
