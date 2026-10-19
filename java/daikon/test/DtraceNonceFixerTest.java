package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.tools.DtraceNonceFixer;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
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

    DtraceNonceFixer.mainHelper(new String[] {in.toString()});

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
    Path out = dir.resolve("in.dtrace_all_fixed");
    String actual = Files.readString(out, UTF_8).replace(System.lineSeparator(), "\n");
    assertEquals(DECLS + expectedSamples, actual);
    // The input file is unchanged.
    assertEquals(DECLS + samples, Files.readString(in, UTF_8));
  }

  /**
   * Runs DtraceNonceFixer on a new input file, and returns the output that includes nonces for all
   * samples.
   *
   * @param input the contents of the input file
   * @return the contents of the output file
   * @throws IOException if there is trouble reading or writing a file
   */
  private String fix(String input) throws IOException {
    Path dir = tmpFolder.newFolder().toPath();
    Path in = dir.resolve("in.dtrace");
    Path out = dir.resolve("in.dtrace_all_fixed");
    Files.writeString(in, input, UTF_8);
    DtraceNonceFixer.mainHelper(new String[] {in.toString()});
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
   * Tests that a compressed input file yields a compressed output file.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testCompressed() throws IOException {
    Path dir = tmpFolder.getRoot().toPath();
    Path in = dir.resolve("in.dtrace.gz");
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
    try (OutputStream fileOs = Files.newOutputStream(in);
        OutputStream os = new GZIPOutputStream(fileOs)) {
      os.write(samples.getBytes(UTF_8));
    }

    DtraceNonceFixer.mainHelper(new String[] {in.toString()});

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
    try (InputStream fileIs = Files.newInputStream(dir.resolve("in.dtrace.gz_all_fixed.gz"));
        InputStream is = new GZIPInputStream(fileIs)) {
      actual = new String(is.readAllBytes(), UTF_8).replace(System.lineSeparator(), "\n");
    }
    assertEquals(expected, actual);
  }

  /**
   * Tests that a nonce header with surrounding whitespace is not treated as a nonce header, because
   * Daikon treats it as a variable name.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testNonceHeaderWithWhitespace() throws IOException {
    String samples =
        String.join("\n", "aprogram.point:::POINT", " this_invocation_nonce", "0", "1", "", "");
    String expected =
        String.join(
            "\n",
            "aprogram.point:::POINT",
            "this_invocation_nonce",
            "1",
            " this_invocation_nonce",
            "0",
            "1",
            "",
            "");
    assertEquals(expected, fix(samples));
  }

  /**
   * Tests that an EXIT sample without a nonce gets the nonce of its ENTER sample, including for
   * nested calls and for a sample that is preceded by a comment.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testMatchEnterExit() throws IOException {
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
    assertEquals(expected, fix(samples));
  }

  /**
   * Tests that an EXIT sample without a nonce does not get the nonce of an ENTER sample that has a
   * nonce, even one that has no EXIT sample, because Daikon does not pair those samples.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testExitWithoutNonceSkipsNoncedEnter() throws IOException {
    String samples =
        String.join(
            "\n",
            "C.f():::ENTER",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "5",
            "",
            "C.f():::EXIT3",
            "",
            "C.f():::EXIT3",
            "",
            "");
    String expected =
        String.join(
            "\n",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "6",
            "",
            "C.f():::ENTER",
            "this_invocation_nonce",
            "5",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "6",
            "",
            "C.f():::EXIT3",
            "this_invocation_nonce",
            "7",
            "",
            "");
    assertEquals(expected, fix(samples));
  }
}
