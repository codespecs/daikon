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
   * Runs Daikon on one sample, in which x is 1, at a program point that is declared as a point, and
   * asserts that Daikon used the sample and wrote an inv file.
   *
   * @param pptName the name of the program point, such as "Foo.bar():::ENTER"
   * @param fileBase the base name of the decls, dtrace, and inv files, such as "point"
   */
  private void runOnePointSample(String pptName, String fileBase) throws IOException {
    File declsFile = tmpFolder.newFile(fileBase + ".decls");
    String decls =
        String.join(
            "\n",
            "decl-version 2.0",
            "var-comparability none",
            "",
            "ppt " + pptName,
            "ppt-type point",
            "variable x",
            "  var-kind variable",
            "  dec-type int",
            "  rep-type int",
            "",
            "");
    Files.write(declsFile.toPath(), decls.getBytes(StandardCharsets.UTF_8));
    File dtraceFile = tmpFolder.newFile(fileBase + ".dtrace");
    String dtrace = pptName + "\nx\n1\n1\n\n";
    Files.write(dtraceFile.toPath(), dtrace.getBytes(StandardCharsets.UTF_8));
    File invFile = new File(tmpFolder.getRoot(), fileBase + ".inv.gz");
    try {
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
      // Also discards any unmatched entry that the sample leaves on the call stack.
      Daikon.cleanup();
    }
  }

  /**
   * Daikon infers invariants from a sample at a point that is named like an enter point. If Daikon
   * did not count the sample as used, it would throw a UserError: "No samples found for any of 1
   * program point".
   */
  @Test
  public void testPointNamedEnterSample() throws IOException {
    runOnePointSample("Foo.bar():::ENTER", "point");
  }

  /**
   * Daikon infers invariants from a sample at a point that is named like an abrupt completion
   * point, even though no entry precedes the sample. If Daikon matched this sample with an entry,
   * it would throw an Error: "Function exit without corresponding entry".
   */
  @Test
  public void testPointNamedThrowsSample() throws IOException {
    runOnePointSample("Foo.bar():::THROWS", "throws");
  }
}
