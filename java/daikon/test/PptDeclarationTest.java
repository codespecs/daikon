package daikon.test;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests the checking of the ppt record of a version 2 declaration. */
public class PptDeclarationTest {

  /** A temporary directory for decls files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /**
   * Tests that a ppt record with anything after the ppt name is a declaration error.
   *
   * @throws IOException if there is trouble writing the decls file
   */
  @Test
  public void testSurplusToken() throws IOException {
    File declsFile = tmpFolder.newFile("test.decls");
    String decls =
        String.join(
            "\n",
            "decl-version 2.0",
            "var-comparability none",
            "",
            "ppt foo():::ENTER unexpected",
            "ppt-type enter",
            "variable x",
            "  var-kind variable",
            "  dec-type int",
            "  rep-type int",
            "");
    Files.write(declsFile.toPath(), decls.getBytes(StandardCharsets.UTF_8));
    List<File> files = Collections.singletonList(declsFile);
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> FileIO.read_declaration_files(files));
    String message = String.valueOf(e.getMessage());
    assertTrue(message, message.startsWith("'unexpected' found where end-of-line expected"));
  }
}
