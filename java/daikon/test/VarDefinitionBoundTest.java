package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.FileIO.VarDefinition;
import daikon.ProglangType;
import daikon.VarInfo.VarKind;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Collections;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Tests the checking of min-value and max-value records in a decls file. */
public class VarDefinitionBoundTest {

  /** A temporary directory for decls files. */
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  /**
   * Returns a variable definition with the given rep type and bounds.
   *
   * @param repType the rep type of the variable
   * @param minValue the min-value bound, or null
   * @param maxValue the max-value bound, or null
   * @return a variable definition with the given rep type and bounds
   */
  private static VarDefinition vardef(
      ProglangType repType, @Nullable String minValue, @Nullable String maxValue) {
    VarDefinition result = new VarDefinition("x", VarKind.VARIABLE, repType);
    result.arr_dims = repType.dimensions();
    result.min_value = minValue;
    result.max_value = maxValue;
    return result;
  }

  /**
   * Asserts that checking the given variable definition fails with the given message.
   *
   * @param vardef the variable definition to check
   * @param expectedMessage the expected message of the AssertionError
   */
  private static void assertInvalid(VarDefinition vardef, String expectedMessage) {
    AssertionError e = assertThrows(AssertionError.class, vardef::checkRep);
    assertEquals(expectedMessage, String.valueOf(e.getMessage()));
  }

  /** Tests bounds that are valid for their rep type. */
  @Test
  public void testValidBounds() {
    vardef(ProglangType.INT, "-9223372036854775808", "9223372036854775807").checkRep();
    vardef(ProglangType.DOUBLE, "0.25", "1e9").checkRep();
    vardef(ProglangType.INT_ARRAY, "0", "100").checkRep();
  }

  /** Tests an integral bound that does not fit in a long. */
  @Test
  public void testIntegralBoundOutOfRange() {
    assertInvalid(
        vardef(ProglangType.INT, null, "18446744073709551615"),
        "max-value '18446744073709551615' for variable x is not a valid int value");
  }

  /** Tests an integral bound that is not an integer. */
  @Test
  public void testIntegralBoundNotInteger() {
    assertInvalid(
        vardef(ProglangType.INT, "100.0", null),
        "min-value '100.0' for variable x is not a valid int value");
  }

  /** Tests that the error message for an array variable names the element type. */
  @Test
  public void testArrayBoundNotInteger() {
    assertInvalid(
        vardef(ProglangType.INT_ARRAY, "1.5", null),
        "min-value '1.5' for variable x is not a valid int value");
  }

  /** Tests a floating-point bound that is not a number. */
  @Test
  public void testFloatBoundNotNumber() {
    assertInvalid(
        vardef(ProglangType.DOUBLE, "abc", null),
        "min-value 'abc' for variable x is not a valid double value");
  }

  /** Tests a floating-point bound that is NaN. */
  @Test
  public void testFloatBoundNaN() {
    assertInvalid(
        vardef(ProglangType.DOUBLE, null, "NaN"),
        "max-value 'NaN' for variable x is not a valid double value");
  }

  /** Tests a bound on a variable whose rep type is not numeric. */
  @Test
  public void testStringBound() {
    assertInvalid(
        vardef(ProglangType.STRING, "abc", null),
        "min-value is not permitted for variable x, whose rep type is java.lang.String");
  }

  /** Tests a variable definition that has no rep type. */
  @Test
  @SuppressWarnings("nullness:assignment") // testing a malformed declaration
  public void testMissingRepType() {
    VarDefinition vardef = vardef(ProglangType.INT, null, null);
    vardef.rep_type = null;
    assertInvalid(vardef, "missing rep-type information for variable x");
  }

  /**
   * Reads a decls file with the given variable records, and returns the message of the resulting
   * UserError.
   *
   * @param varRecords the records of variable x, one per line
   * @return the message of the UserError that reading the decls file throws
   * @throws IOException if there is trouble writing the decls file
   */
  private String readDeclsError(String... varRecords) throws IOException {
    File declsFile = tmpFolder.newFile("test.decls");
    StringBuilder decls = new StringBuilder();
    decls.append("decl-version 2.0\n");
    decls.append("var-comparability none\n");
    decls.append("\n");
    decls.append("ppt foo:::ENTER\n");
    decls.append("ppt-type enter\n");
    decls.append("variable x\n");
    for (String varRecord : varRecords) {
      decls.append("  ").append(varRecord).append("\n");
    }
    Files.write(declsFile.toPath(), decls.toString().getBytes(StandardCharsets.UTF_8));
    FileIO.resetNewDeclFormat();
    List<File> files = Collections.singletonList(declsFile);
    Daikon.UserError e =
        assertThrows(Daikon.UserError.class, () -> FileIO.read_declaration_files(files));
    return String.valueOf(e.getMessage());
  }

  /** Tests that an invalid bound is reported at the line of the bound record. */
  @Test
  public void testErrorLine() throws IOException {
    String message =
        readDeclsError("var-kind variable", "rep-type double", "dec-type double", "max-value 1.5x");
    assertTrue(
        message,
        message.startsWith(
            "max-value '1.5x' for variable x is not a valid double value at line 10 in file "));
  }

  /** Tests that an invalid bound that precedes the rep-type record is reported. */
  @Test
  public void testBoundBeforeRepType() throws IOException {
    String message =
        readDeclsError("var-kind variable", "max-value abc", "rep-type double", "dec-type double");
    assertTrue(
        message,
        message.startsWith("max-value 'abc' for variable x is not a valid double value at line "));
  }
}
