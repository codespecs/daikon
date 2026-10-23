package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
    String enter = "C.m(int):::ENTER";
    String exit = "C.m(int):::EXIT1";
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
    try {
      Path input = dir.resolve("test.dtrace");
      Path output = dir.resolve("test.second-half.dtrace");
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
      Files.delete(input);
      Files.delete(output);
    } finally {
      Files.delete(dir);
    }
  }

  /**
   * Returns a declaration of a ppt with one variable, in the version 1 format.
   *
   * @param pptName the name of the ppt
   * @return a declaration of a ppt with one variable
   */
  private static String declarationVersion1(String pptName) {
    return String.join(System.lineSeparator(), "DECLARE", pptName, "x", "int", "int", "1", "");
  }

  /**
   * Tests that SplitDtrace treats a sample of a version 1 file as a sample, even if the name of its
   * ppt starts with "ppt ", like a version 2 declaration.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testVersion1PptNameStartingWithPpt() throws IOException {
    String enter = "ppt C.m(int):::ENTER";
    String exit = "ppt C.m(int):::EXIT1";
    StringJoiner dtrace = new StringJoiner(System.lineSeparator());
    dtrace.add("decl-version 1.0");
    dtrace.add("");
    dtrace.add(declarationVersion1(enter));
    dtrace.add(declarationVersion1(exit));
    for (int nonce = 1; nonce <= 4; nonce++) {
      dtrace.add(sample(enter, nonce));
      dtrace.add(sample(exit, nonce));
    }

    Path dir = Files.createTempDirectory("SplitDtraceTest");
    try {
      Path input = dir.resolve("test.dtrace");
      Path output = dir.resolve("test.second-half.dtrace");
      Files.write(input, dtrace.toString().getBytes(UTF_8));
      SplitDtrace.mainHelper(new String[] {input.toString()});

      String result = new String(Files.readAllBytes(output), UTF_8);
      String nl = System.lineSeparator();
      assertTrue(result, result.contains("DECLARE" + nl + enter + nl));
      assertTrue(result, result.contains("DECLARE" + nl + exit + nl));
      assertFalse(result, result.contains("this_invocation_nonce" + nl + "2" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "3" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "4" + nl));
      Files.delete(input);
      Files.delete(output);
    } finally {
      Files.delete(dir);
    }
  }

  /**
   * Tests that a comment immediately followed by a sample, with no blank line between them, does
   * not make SplitDtrace copy the sample when it is not in the selected half.
   *
   * @throws IOException if there is trouble reading or writing a file
   */
  @Test
  public void testCommentBeforeSample() throws IOException {
    String enter = "C.m(int):::ENTER";
    String exit = "C.m(int):::EXIT1";
    StringJoiner dtrace = new StringJoiner(System.lineSeparator());
    dtrace.add("decl-version 2.0");
    dtrace.add("var-comparability implicit");
    dtrace.add("");
    dtrace.add(declaration(enter, "enter"));
    dtrace.add(declaration(exit, "subexit"));
    for (int nonce = 1; nonce <= 4; nonce++) {
      if (nonce == 2) {
        // A comment that immediately precedes a sample in the first half.
        dtrace.add("// A comment" + System.lineSeparator() + sample(enter, nonce));
      } else {
        dtrace.add(sample(enter, nonce));
      }
      dtrace.add(sample(exit, nonce));
    }

    Path dir = Files.createTempDirectory("SplitDtraceTest");
    try {
      Path input = dir.resolve("test.dtrace");
      Path output = dir.resolve("test.second-half.dtrace");
      Files.write(input, dtrace.toString().getBytes(UTF_8));
      SplitDtrace.mainHelper(new String[] {input.toString()});

      String result = new String(Files.readAllBytes(output), UTF_8);
      String nl = System.lineSeparator();
      assertTrue(result, result.contains("// A comment" + nl));
      assertFalse(result, result.contains("this_invocation_nonce" + nl + "2" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "3" + nl));
      assertTrue(result, result.contains("this_invocation_nonce" + nl + "4" + nl));
      Files.delete(input);
      Files.delete(output);
    } finally {
      Files.delete(dir);
    }
  }
}
