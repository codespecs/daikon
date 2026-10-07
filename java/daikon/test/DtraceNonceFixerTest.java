package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.tools.DtraceNonceFixer;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
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

  /**
   * Runs DtraceNonceFixer with a new input file and a separate output file, and returns the output.
   *
   * @param input the contents of the input file
   * @return the contents of the output file
   * @throws IOException if there is trouble reading or writing a file
   */
  private String fix(String input) throws IOException {
    Path dir = tmpFolder.newFolder().toPath();
    Path in = dir.resolve("in.dtrace");
    Path out = dir.resolve("out.dtrace");
    Files.writeString(in, input, UTF_8);
    DtraceNonceFixer.mainHelper(new String[] {in.toString(), out.toString()});
    return Files.readString(out, UTF_8).replace(System.lineSeparator(), "\n");
  }

  /**
   * Tests that a THROWS sample with nonce 0 is not treated as the start of a concatenated file.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testThrowsWithNonceZero() throws IOException {
    String samples =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::THROWS",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "1",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "1",
            "",
            // A second, concatenated file.
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "0",
            "",
            "");
    String expected =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::THROWS",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "1",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "1",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "2",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "2",
            "",
            "");
    assertEquals(expected, fix(samples));
  }

  /**
   * Tests that an EXIT sample without a nonce gets the nonce of its ENTER sample, when the ENTER
   * sample has a nonce. Also tests that an EXIT sample with a nonce matches its ENTER sample.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testEnterWithNonceExitWithout() throws IOException {
    String samples =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "5",
            "",
            "C.g():::ENTER",
            "this_invocation_nonce",
            "6",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "7",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "7",
            "",
            "C.g():::EXIT9",
            "",
            "C.f():::EXIT3",
            "",
            "");
    String expected =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "5",
            "",
            "C.g():::ENTER",
            "this_invocation_nonce",
            "6",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "7",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "7",
            "",
            "C.g():::EXIT9",
            "this_invocation_nonce",
            "6",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "5",
            "",
            "");
    assertEquals(expected, fix(samples));
  }

  /**
   * Tests that a line that contains only whitespace does not end a sample, and that whitespace is
   * preserved.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testWhitespaceLine() throws IOException {
    String samples = String.join("\n", "aprogram.point:::POINT ", "x", "   ", "1", "", "");
    String expected =
        String.join(
            "\n", "aprogram.point:::POINT ", "this_invocation_nonce", "1", "x", "   ", "1", "", "");
    assertEquals(expected, fix(samples));
  }

  /**
   * Tests that a compressed file can be both the input and the output.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testInPlaceCompressed() throws IOException {
    Path dir = tmpFolder.getRoot().toPath();
    Path file = dir.resolve("in.dtrace.gz");
    String samples =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "0",
            "",
            "");
    try (OutputStream os = new GZIPOutputStream(Files.newOutputStream(file))) {
      os.write(samples.getBytes(UTF_8));
    }

    DtraceNonceFixer.mainHelper(new String[] {file.toString(), file.toString()});

    String expected =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "0",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "1",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "1",
            "",
            "");
    String actual;
    try (InputStream is = new GZIPInputStream(Files.newInputStream(file))) {
      actual = new String(is.readAllBytes(), UTF_8).replace(System.lineSeparator(), "\n");
    }
    assertEquals(expected, actual);
    // No temporary file remains.
    try (Stream<Path> files = Files.list(dir)) {
      assertEquals(1, files.count());
    }
  }
}
