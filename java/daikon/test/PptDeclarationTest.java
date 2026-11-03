package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.PptMap;
import java.io.IOException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests the reading of the ppt record of a version 2 declaration. */
public class PptDeclarationTest {

  /** A temporary directory for decls files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /**
   * Returns the program points read from a decls file that contains one declaration.
   *
   * @param pptRecord the ppt record of the declaration, such as {@code "ppt foo():::ENTER"}
   * @return the program points read from the decls file
   * @throws IOException if there is trouble writing or reading the decls file
   */
  private PptMap readDecl(String pptRecord) throws IOException {
    return Common.readDecls(
        tmpFolder.newFile(),
        "decl-version 2.0",
        "var-comparability none",
        "",
        pptRecord,
        "ppt-type enter",
        "variable x",
        "  var-kind variable",
        "  dec-type int",
        "  rep-type int");
  }

  /**
   * Tests that a ppt name with unescaped blanks is read in full.
   *
   * @throws IOException if there is trouble writing or reading the decls file
   */
  @Test
  public void testUnescapedBlanks() throws IOException {
    PptMap ppts = readDecl("ppt Year.compute(int, int, int):::ENTER");
    assertEquals(1, ppts.size());
    assertTrue(ppts.containsName("Year.compute(int, int, int):::ENTER"));
  }

  /**
   * Tests that a ppt name with escaped blanks, as Chicory writes it, is unescaped.
   *
   * @throws IOException if there is trouble writing or reading the decls file
   */
  @Test
  public void testEscapedBlanks() throws IOException {
    PptMap ppts = readDecl("ppt Year.compute(int,\\_int,\\_int):::ENTER  ");
    assertEquals(1, ppts.size());
    assertTrue(ppts.containsName("Year.compute(int, int, int):::ENTER"));
  }
}
