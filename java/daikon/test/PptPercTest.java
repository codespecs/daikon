package daikon.test;

import static java.nio.charset.StandardCharsets.UTF_8;
import static org.junit.Assert.assertEquals;

import daikon.Daikon;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.StringJoiner;
import org.junit.Test;

/** Tests Daikon's --ppt-perc option, which chooses the program points to process. */
public class PptPercTest {

  /**
   * Returns the result of Daikon.setup_ppt_perc, the name of the last program point to process.
   *
   * @param decls the contents of a declaration file
   * @param pptPerc the percentage of program points to process
   * @return the name of the last program point to process
   * @throws Exception if there is trouble writing the file or calling the method
   */
  private static String maxPptName(String decls, int pptPerc) throws Exception {
    Path file = Files.createTempFile("PptPercTest", ".decls");
    try {
      Files.write(file, decls.getBytes(UTF_8));
      Method setupPptPerc =
          Daikon.class.getDeclaredMethod("setup_ppt_perc", Collection.class, int.class);
      setupPptPerc.setAccessible(true);
      return (String) setupPptPerc.invoke(null, Collections.singletonList(file.toFile()), pptPerc);
    } finally {
      Files.delete(file);
    }
  }

  /**
   * Returns a version 1 declaration of a program point with one variable.
   *
   * @param pptName the name of the program point
   * @param varName the name of the variable
   * @return a version 1 declaration
   */
  private static String declarationVersion1(String pptName, String varName) {
    return String.join(System.lineSeparator(), "DECLARE", pptName, varName, "int", "int", "1", "");
  }

  /**
   * Tests that, in a version 1 file, a variable whose name looks like a version 2 declaration or a
   * version 1 declaration is not taken for a program point.
   *
   * @throws Exception if there is trouble writing the file or calling the method
   */
  @Test
  public void testVersion1VariableNames() throws Exception {
    StringJoiner decls = new StringJoiner(System.lineSeparator());
    decls.add(declarationVersion1("B.m():::ENTER", "ppt zzz"));
    decls.add(declarationVersion1("B.m():::EXIT1", "DECLARE"));
    decls.add(declarationVersion1("C.m():::ENTER", "x"));
    decls.add(declarationVersion1("C.m():::EXIT1", "x"));
    // Of the 4 program points, 60% is 2, so processing stops after B.m():::EXIT1.  If "zzz" were
    // a program point, then processing would stop after the 3rd of 5, and so after C.m().
    assertEquals("B.m():::EXIT1", maxPptName(decls.toString(), 60));
  }
}
