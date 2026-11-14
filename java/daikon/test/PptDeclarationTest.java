package daikon.test;

import static org.junit.Assert.assertEquals;

import daikon.PptMap;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests the reading of the ppt record of a version 2 declaration. */
public class PptDeclarationTest {

  /** A temporary directory for decls files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /**
   * Returns the program points that Daikon reads from the declaration that starts with the given
   * ppt record.
   *
   * @param pptRecord a ppt record, such as {@code "ppt foo():::ENTER"}
   * @return the program points read from the declaration
   * @throws IOException if there is trouble writing or reading the decls file
   */
  private PptMap readDecl(String pptRecord) throws IOException {
    return Common.readDecls(
        tmpFolder.newFile(),
        Arrays.asList(
            "decl-version 2.0",
            "var-comparability none",
            "",
            pptRecord,
            "ppt-type enter",
            "variable x",
            "  var-kind variable",
            "  dec-type int",
            "  rep-type int"));
  }

  /**
   * Tests that a ppt name with unescaped blanks is read in full.
   *
   * @throws IOException if there is trouble writing or reading the decls file
   */
  @Test
  public void testUnescapedBlanks() throws IOException {
    PptMap ppts = readDecl("ppt Year.compute(int, int, int):::ENTER");
    assertEquals(
        Collections.singletonList("Year.compute(int, int, int):::ENTER"),
        new ArrayList<>(ppts.nameStringSet()));
  }

  /**
   * Tests that a ppt name with escaped blanks, as Chicory writes it, is unescaped.
   *
   * @throws IOException if there is trouble writing or reading the decls file
   */
  @Test
  public void testEscapedBlanks() throws IOException {
    PptMap ppts = readDecl("ppt Year.compute(int,\\_int,\\_int):::ENTER  ");
    assertEquals(
        Collections.singletonList("Year.compute(int, int, int):::ENTER"),
        new ArrayList<>(ppts.nameStringSet()));
  }
}
