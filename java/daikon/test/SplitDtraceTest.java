package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.SplitDtrace;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.StringJoiner;
import org.junit.Test;

/** Tests {@link SplitDtrace}. */
public class SplitDtraceTest {

  /**
   * Returns a declaration of a ppt with one variable, in the version 2 format.
   *
   * @param pptName the name of the ppt
   * @param pptType the type of the ppt
   * @return a declaration of a ppt with one variable
   */
  private static String declaration(String pptName, String pptType) {
    return String.join(
        System.lineSeparator(),
        "ppt " + pptName,
        "ppt-type " + pptType,
        "variable x",
        "  var-kind variable",
        "  dec-type int",
        "  rep-type int",
        "  comparability 1",
        "");
  }

  /**
   * Returns a sample for a ppt declared by {@link #declaration}.
   *
   * @param pptName the name of the ppt
   * @param nonce the invocation nonce, which is also the value of the variable
   * @return a sample for the ppt
   */
  private static String sample(String pptName, int nonce) {
    return String.join(
        System.lineSeparator(),
        pptName,
        "this_invocation_nonce",
        String.valueOf(nonce),
        "x",
        String.valueOf(nonce),
        "1",
        "");
  }

  /**
   * Tests that SplitDtrace writes the header records and declarations of a version 2 file, and the
   * second half of its samples.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testVersion2() throws IOException {
    checkSecondHalf("C.m(int):::ENTER", "C.m(int):::EXIT1");
  }

  /**
   * Tests that SplitDtrace treats a sample as a sample, even if the name of its program point
   * starts with the keyword of a header record.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testPptNameStartingWithKeyword() throws IOException {
    checkSecondHalf("decl-version.C.m(int):::ENTER", "var-comparability.C.m(int):::EXIT1");
    checkSecondHalf("input-language.C.m(int):::ENTER", "input-language.C.m(int):::EXIT1");
  }

  /**
   * Checks that SplitDtrace writes the header records and declarations of a version 2 file with an
   * entry and an exit program point, and the second half of its samples.
   *
   * @param enter the name of the entry program point
   * @param exit the name of the exit program point
   * @throws IOException if there is trouble reading or writing a file
   */
  private static void checkSecondHalf(String enter, String exit) throws IOException {
    StringJoiner dtrace = new StringJoiner(System.lineSeparator());
    dtrace.add("// A comment");
    dtrace.add("");
    dtrace.add("decl-version 2.0");
    dtrace.add("var-comparability implicit");
    dtrace.add("");
    dtrace.add(declaration(enter, "enter"));
    dtrace.add(declaration(exit, "subexit"));
    for (int nonce = 1; nonce <= 4; nonce++) {
      dtrace.add(sample(enter, nonce));
      dtrace.add(sample(exit, nonce));
    }

    Path dir = Files.createTempDirectory("SplitDtraceTest");
    Path input = dir.resolve("test.dtrace");
    Path output = dir.resolve("test.second-half.dtrace");
    try {
      Files.write(input, dtrace.toString().getBytes(UTF_8));
      SplitDtrace.mainHelper(new String[] {input.toString()});

      String result = new String(Files.readAllBytes(output), UTF_8);
      String nl = System.lineSeparator();
      assertTrue(result, result.contains("decl-version 2.0" + nl));
      assertTrue(result, result.contains("ppt " + enter + nl));
      assertTrue(result, result.contains("ppt " + exit + nl));
      assertFalse(result, result.contains("this_invocation_nonce" + nl + "2" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "3" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "4" + nl));
    } finally {
      // Deleting the files in the finally block keeps a failed assertion from being hidden by a
      // failure to delete the nonempty directory.
      Files.deleteIfExists(input);
      Files.deleteIfExists(output);
      Files.delete(dir);
    }
  }

  /**
   * Tests that SplitDtrace rejects a file in the obsolete version 1 format, rather than writing an
   * output file without its declarations.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testVersion1() throws IOException {
    String enter = "C.m(int):::ENTER";
    String declare =
        String.join(System.lineSeparator(), "DECLARE", enter, "x", "int", "int", "1", "");
    String version1 = "decl-version 1.0" + System.lineSeparator();
    for (String header : new String[] {declare, version1}) {
      Path dir = Files.createTempDirectory("SplitDtraceTest");
      Path input = dir.resolve("test.dtrace");
      Path output = dir.resolve("test.second-half.dtrace");
      try {
        Files.write(input, (header + System.lineSeparator() + sample(enter, 1)).getBytes(UTF_8));
        Daikon.UserError e =
            assertThrows(
                Daikon.UserError.class,
                () -> SplitDtrace.mainHelper(new String[] {input.toString()}));
        String message = String.valueOf(e.getMessage());
        assertTrue(message, message.contains("version 1 format"));
        assertFalse(Files.exists(output));
      } finally {
        Files.deleteIfExists(input);
        Files.deleteIfExists(output);
        Files.delete(dir);
      }
    }
  }
}
