package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.tools.DtraceNonceFixer;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests {@link DtraceNonceFixer}. */
public class DtraceNonceFixerTest {

  /** A directory for the test's files, which is deleted after each test. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /** Declarations, in the new format, which DtraceNonceFixer must not change. */
  static final String DECLS =
      String.join(
          "\n",
          "decl-version 2.0",
          "var-comparability none",
          "",
          "ppt aprogram.point:::POINT",
          "ppt-type point",
          "variable x",
          "  var-kind variable",
          "  dec-type int",
          "  rep-type int",
          "",
          "");

  /**
   * Tests that samples without nonces get nonces, without losing their first variable, and that
   * declarations are unchanged.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testAddNonces() throws IOException {
    Path dir = tmpFolder.getRoot().toPath();
    Path in = dir.resolve("in.dtrace");
    Path out = dir.resolve("out.dtrace");
    String samples =
        String.join(
            "\n",
            "aprogram.point:::POINT",
            "x",
            "5",
            "1",
            "",
            "aprogram.point:::POINT",
            "x",
            "6",
            "1",
            "",
            "");
    Files.writeString(in, DECLS + samples, UTF_8);

    DtraceNonceFixer.mainHelper(new String[] {in.toString(), out.toString()});

    String expectedSamples =
        String.join(
            "\n",
            "aprogram.point:::POINT",
            "this_invocation_nonce",
            "1",
            "x",
            "5",
            "1",
            "",
            "aprogram.point:::POINT",
            "this_invocation_nonce",
            "2",
            "x",
            "6",
            "1",
            "",
            "");
    String actual = Files.readString(out, UTF_8).replace(System.lineSeparator(), "\n");
    assertEquals(DECLS + expectedSamples, actual);
    // The input file is unchanged, and no intermediate file remains.
    assertEquals(DECLS + samples, Files.readString(in, UTF_8));
    try (Stream<Path> files = Files.list(dir)) {
      assertEquals(2, files.count());
    }
  }

  /**
   * Tests that an EXIT sample without a nonce gets the nonce of its ENTER sample, including for
   * nested calls and for a sample that is preceded by a comment.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testMatchEnterExit() throws IOException {
    Path dir = tmpFolder.getRoot().toPath();
    Path in = dir.resolve("in.dtrace");
    Path out = dir.resolve("out.dtrace");
    String samples =
        String.join(
            "\n",
            "C.f():::ENTER",
            "",
            "C.g():::ENTER",
            "",
            "# a comment",
            "C.g():::EXIT7",
            "",
            "C.f():::EXIT3",
            "",
            "");
    Files.writeString(in, samples, UTF_8);

    DtraceNonceFixer.mainHelper(new String[] {in.toString(), out.toString()});

    String expected =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "1",
            "",
            "C.g():::ENTER",
            "this_invocation_nonce",
            "2",
            "",
            "# a comment",
            "C.g():::EXIT7",
            "this_invocation_nonce",
            "2",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "1",
            "",
            "");
    assertEquals(expected, Files.readString(out, UTF_8).replace(System.lineSeparator(), "\n"));
  }
}
