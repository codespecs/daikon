package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.Daikon;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.StringJoiner;
import org.junit.Test;

/**
 * Tests the configuration option daikon.Daikon.ppt_perc, which chooses the program points to
 * process.
 */
public class PptPercTest {

  /**
   * Returns the result of {@link Daikon#setup_ppt_perc}, the name of the last program point to
   * process.
   *
   * @param decls the contents of the declaration files
   * @param dtraces the contents of the data trace files
   * @param pptPerc a percentage of program points to process
   * @return the name of the last program point to process
   * @throws IOException if there is trouble writing a file
   */
  private static String maxPptName(List<String> decls, List<String> dtraces, int pptPerc)
      throws IOException {
    List<Path> files = new ArrayList<>();
    try {
      List<File> declFiles = new ArrayList<>();
      for (String contents : decls) {
        Path file = Files.createTempFile("PptPercTest", ".decls");
        files.add(file);
        Files.write(file, contents.getBytes(UTF_8));
        declFiles.add(file.toFile());
      }
      List<String> dtraceFiles = new ArrayList<>();
      for (String contents : dtraces) {
        Path file = Files.createTempFile("PptPercTest", ".dtrace");
        files.add(file);
        Files.write(file, contents.getBytes(UTF_8));
        dtraceFiles.add(file.toString());
      }
      String result = Daikon.setup_ppt_perc(declFiles, dtraceFiles, pptPerc);
      if (result == null) {
        throw new AssertionError("setup_ppt_perc returned null for ppt_perc " + pptPerc);
      }
      return result;
    } finally {
      for (Path file : files) {
        Files.delete(file);
      }
    }
  }

  /**
   * Returns a version 2 declaration of a program point with one variable.
   *
   * @param pptName the name of the program point
   * @return a version 2 declaration
   */
  private static String declarationVersion2(String pptName) {
    return String.join(
        System.lineSeparator(),
        "ppt " + pptName,
        "ppt-type point",
        "variable x",
        "  var-kind variable",
        "  dec-type int",
        "  rep-type int",
        "  comparability 1",
        "");
  }

  /**
   * Tests a version 2 data trace file that contains the declarations, with no declaration file. Its
   * header records are on consecutive lines, as in the output of Kvasir.
   *
   * @throws IOException if there is trouble writing a file
   */
  @Test
  public void testVersion2DeclarationsInDtrace() throws IOException {
    StringJoiner dtrace = new StringJoiner(System.lineSeparator());
    dtrace.add("input-language C/C++");
    dtrace.add("decl-version 2.0");
    dtrace.add("var-comparability none");
    dtrace.add("");
    dtrace.add(declarationVersion2("B:::OBJECT"));
    dtrace.add(declarationVersion2("B.m():::ENTER"));
    dtrace.add(declarationVersion2("B.m():::EXIT1"));
    dtrace.add(declarationVersion2("C.m():::ENTER"));
    dtrace.add(declarationVersion2("C.m():::EXIT1"));
    dtrace.add(String.join(System.lineSeparator(), "B.m():::ENTER", "x", "1", "1", ""));
    // Of the 4 program points other than B:::OBJECT, 50% is 2, so processing stops after
    // B.m():::EXIT1.
    assertEquals(
        "B.m():::EXIT1",
        maxPptName(Collections.emptyList(), Collections.singletonList(dtrace.toString()), 50));
  }

  /**
   * Tests that a file without a "decl-version" record is in the version 2 format.
   *
   * @throws IOException if there is trouble writing a file
   */
  @Test
  public void testNoDeclVersion() throws IOException {
    StringJoiner decls = new StringJoiner(System.lineSeparator());
    decls.add(declarationVersion2("B.m():::ENTER"));
    decls.add(declarationVersion2("B.m():::EXIT1"));
    decls.add(declarationVersion2("C.m():::ENTER"));
    decls.add(declarationVersion2("C.m():::EXIT1"));
    // Of the 4 program points, 75% is 3, so processing stops after C.m():::EXIT1.
    assertEquals(
        "C.m():::EXIT1",
        maxPptName(Collections.singletonList(decls.toString()), Collections.emptyList(), 75));
  }

  /**
   * Tests program point names that contain unescaped blanks.
   *
   * @throws IOException if there is trouble writing a file
   */
  @Test
  public void testUnescapedBlanks() throws IOException {
    StringJoiner decls = new StringJoiner(System.lineSeparator());
    decls.add(declarationVersion2("B.m(int, int):::ENTER"));
    decls.add(declarationVersion2("B.m(int, int):::EXIT1"));
    decls.add(declarationVersion2("C.m(int, int):::ENTER"));
    decls.add(declarationVersion2("C.m(int, int):::EXIT1"));
    // Of the 4 program points, 50% is 2, so processing stops after B.m(int, int):::EXIT1.
    assertEquals(
        "B.m(int, int):::EXIT1",
        maxPptName(Collections.singletonList(decls.toString()), Collections.emptyList(), 50));
  }
}
