package daikon.test;

import static daikon.tools.nullness.NullnessUtil.castNonNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.chicory.DeclReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/** Tests for {@link DeclReader}. */
public class DeclReaderTest {

  /** The header of each declaration file. */
  private static final List<String> HEADER =
      Arrays.asList("// Declarations", "", "decl-version 2.0", "var-comparability implicit", "");

  /**
   * Returns the declaration of a variable.
   *
   * @param name the variable name
   * @param comparability the comparability, or null for none
   * @return the lines of the variable declaration
   */
  private static List<String> var(String name, @Nullable String comparability) {
    List<String> result = new ArrayList<>();
    result.add("variable " + name);
    result.add("  var-kind variable");
    result.add("  dec-type int");
    result.add("  rep-type int");
    if (comparability != null) {
      result.add("  comparability " + comparability);
    }
    return result;
  }

  /**
   * Returns the declaration of a program point "C.m():::ENTER" whose variables are named a, b, c,
   * ... and have the given comparabilities.
   *
   * @param comparabilities the comparabilities of the variables; null means no comparability record
   * @return the lines of the program point declaration, including the terminating blank line
   */
  private static List<String> ppt(@Nullable String... comparabilities) {
    return namedPpt("C.m():::ENTER", comparabilities);
  }

  /**
   * Returns the declaration of a program point whose variables are named a, b, c, ... and have the
   * given comparabilities.
   *
   * @param pptName the program point name
   * @param comparabilities the comparabilities of the variables; null means no comparability record
   * @return the lines of the program point declaration, including the terminating blank line
   */
  private static List<String> namedPpt(String pptName, @Nullable String... comparabilities) {
    List<String> result = new ArrayList<>();
    result.add("ppt " + pptName);
    result.add("ppt-type enter");
    for (int i = 0; i < comparabilities.length; i++) {
      result.addAll(var(Character.toString('a' + i), comparabilities[i]));
    }
    result.add("");
    return result;
  }

  /**
   * Returns the lines of a declaration file containing the given program points.
   *
   * @param ppts the program point declarations
   * @return the lines of a declaration file
   */
  @SafeVarargs
  private static List<String> file(List<String>... ppts) {
    List<String> result = new ArrayList<>(HEADER);
    for (List<String> ppt : ppts) {
      result.addAll(ppt);
    }
    return result;
  }

  /**
   * Parses the contents of a declaration file.
   *
   * @param lines the lines of the file
   * @return the contents of the file
   */
  private static DeclReader parse(List<String> lines) {
    DeclReader result = new DeclReader();
    try {
      result.read(new StringReader(String.join("\n", lines)), "a");
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    return result;
  }

  /**
   * Returns the comparability of a variable in program point "C.m():::ENTER".
   *
   * @param reader a DeclReader that has read a file
   * @param varName the variable name
   * @return the comparability of the variable, or null if it has none
   */
  private static @Nullable String comparability(DeclReader reader, String varName) {
    DeclReader.DeclPpt ppt = castNonNull(reader.find_ppt("C.m():::ENTER"));
    return castNonNull(ppt.find_var(varName)).get_comparability();
  }

  /** DeclReader skips sample records, and a later declaration of a ppt replaces an earlier one. */
  @Test
  public void testSampleRecordsAndDuplicatePpt() {
    List<String> lines = new ArrayList<>(file(ppt("1", "2"), ppt("3", "3")));
    lines.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "a", "3", "1", ""));
    DeclReader reader = parse(lines);
    assertEquals(1, reader.ppts.size());
    assertEquals("3", comparability(reader, "a"));
  }

  /** Any number of records, such as parent and flags records, may precede the first variable. */
  @Test
  public void testPptRecords() {
    List<String> lines = new ArrayList<>(file(ppt("1", "2")));
    int index = lines.indexOf("variable a");
    lines.add(index, "parent parent C:::OBJECT 1");
    lines.add(index, "flags static");
    DeclReader reader = parse(lines);
    assertEquals("1", comparability(reader, "a"));
    assertEquals("2", comparability(reader, "b"));
  }

  /** A program point with no variables is permitted. */
  @Test
  public void testNoVariables() {
    DeclReader reader = parse(file(ppt(), namedPpt("C.n():::ENTER", "1")));
    assertEquals(0, castNonNull(reader.find_ppt("C.m():::ENTER")).vars.size());
    assertEquals(1, castNonNull(reader.find_ppt("C.n():::ENTER")).vars.size());
  }

  /** A variable with no comparability record has a null comparability. */
  @Test
  public void testMissingComparability() {
    DeclReader reader = parse(file(ppt(null, "2")));
    assertNull(comparability(reader, "a"));
    assertEquals("2", comparability(reader, "b"));
  }

  /** A later declaration of a variable or of a comparability replaces an earlier one. */
  @Test
  public void testDuplicateVariableAndComparability() {
    List<String> twice = new ArrayList<>(file(ppt("1")));
    twice.addAll(twice.size() - 1, var("a", "2"));
    twice.add(twice.size() - 1, "  comparability 3");
    assertEquals("3", comparability(parse(twice), "a"));
  }

  /** A later declaration of a variable keeps the index of the earlier one. */
  @Test
  public void testDuplicateVariableIndex() {
    List<String> twice = new ArrayList<>(file(ppt("1", "2")));
    twice.addAll(twice.size() - 1, var("a", "3"));
    DeclReader.DeclPpt ppt = castNonNull(parse(twice).find_ppt("C.m():::ENTER"));
    assertEquals(0, castNonNull(ppt.find_var("a")).index);
    assertEquals(1, castNonNull(ppt.find_var("b")).index);
  }

  /** DeclReader skips unrecognized records and ignores extra tokens. */
  @Test
  public void testLenient() {
    List<String> unknownPptRecord = new ArrayList<>(file(ppt("1")));
    unknownPptRecord.add(unknownPptRecord.indexOf("variable a"), "new-ppt-record x");
    List<String> unknownVarRecord = new ArrayList<>(file(ppt("1")));
    unknownVarRecord.add(unknownVarRecord.size() - 1, "  new-var-record x");
    List<String> extraComparabilityToken = new ArrayList<>(file(ppt("1")));
    extraComparabilityToken.set(
        extraComparabilityToken.indexOf("  comparability 1"), "  comparability 1 x");
    List<String> extraVariableToken = new ArrayList<>(file(ppt("1")));
    extraVariableToken.set(extraVariableToken.indexOf("variable a"), "variable a x");
    List<String> noColons = new ArrayList<>(file(ppt("1")));
    noColons.set(noColons.indexOf("ppt C.m():::ENTER"), "ppt C.m()");
    for (List<String> lines :
        Arrays.asList(
            unknownPptRecord,
            unknownVarRecord,
            extraComparabilityToken,
            extraVariableToken,
            noColons)) {
      DeclReader reader = parse(lines);
      DeclReader.DeclPpt ppt = reader.ppts.values().iterator().next();
      DeclReader.DeclVarInfo var = castNonNull(ppt.find_var("a"));
      assertEquals("1", var.get_comparability());
    }
  }

  /** Tokens may be separated by tabs or by multiple spaces. */
  @Test
  public void testWhitespace() {
    List<String> lines = new ArrayList<>(file(ppt("2", "3")));
    lines.set(lines.indexOf("ppt C.m():::ENTER"), "ppt\tC.m():::ENTER");
    lines.set(lines.indexOf("variable a"), "variable\ta");
    lines.set(lines.indexOf("  comparability 2"), "\tcomparability  2");
    DeclReader reader = parse(lines);
    assertEquals("2", comparability(reader, "a"));
    assertEquals("3", comparability(reader, "b"));
  }

  /** DeclReader skips a sample record for a variable named "ppt". */
  @Test
  public void testSampleVariableNamedPpt() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "ppt", "3", "1", ""));
    DeclReader reader = parse(lines);
    assertEquals(1, reader.ppts.size());
  }

  /** A program point declaration that is not ended by a blank line is rejected. */
  @Test
  public void testMissingBlankLineBetweenPpts() {
    List<String> lines = new ArrayList<>(HEADER);
    lines.addAll(namedPpt("C.m():::ENTER"));
    lines.remove(lines.size() - 1);
    lines.addAll(namedPpt("C.m():::EXIT1", "1"));
    assertThrows(Daikon.UserError.class, () -> parse(lines));
  }

  /** A "ppt" record within a variable declaration is rejected. */
  @Test
  public void testPptRecordInVariable() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.remove(lines.size() - 1);
    lines.addAll(namedPpt("C.m():::EXIT1", "1"));
    assertThrows(Daikon.UserError.class, () -> parse(lines));
  }

  /** A variable without a rep-type record is rejected. */
  @Test
  public void testMissingRepType() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.remove("  rep-type int");
    assertThrows(Daikon.UserError.class, () -> parse(lines));
  }
}
