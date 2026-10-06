package daikon.test;

import static daikon.tools.nullness.NullnessUtil.castNonNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.chicory.DeclReader;
import daikon.tools.MergeComparability;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/** Tests for {@link MergeComparability}. */
public class MergeComparabilityTest {

  /** The header of each declaration file. */
  private static final List<String> HEADER =
      Arrays.asList("// Declarations", "", "decl-version 2.0", "var-comparability implicit", "");

  /** The header of the merged output, for input files named "a" and "b". */
  private static final List<String> MERGED_HEADER =
      Arrays.asList(
          "// Declarations written by daikon.tools.MergeComparability, merging:",
          "//   a",
          "//   b",
          "",
          "decl-version 2.0",
          "var-comparability implicit",
          "");

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
   * Parses the contents of a declaration file, for rewriting.
   *
   * @param filename the file name, used in error messages
   * @param lines the lines of the file
   * @return the contents of the file
   */
  private static DeclReader parse(String filename, List<String> lines) {
    return parse(filename, lines, true);
  }

  /**
   * Parses the contents of a declaration file.
   *
   * @param filename the file name, used in error messages
   * @param lines the lines of the file
   * @param forRewriting if true, create a DeclReader for rewriting
   * @return the contents of the file
   */
  private static DeclReader parse(String filename, List<String> lines, boolean forRewriting) {
    DeclReader result = new DeclReader(forRewriting);
    try {
      result.read(new StringReader(String.join("\n", lines)), filename);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
    return result;
  }

  /**
   * Merges two declaration files, named "a" and "b".
   *
   * @param a the lines of the first file
   * @param b the lines of the second file
   * @return the lines of the merged output
   */
  private static List<String> merge(List<String> a, List<String> b) {
    StringWriter sw = new StringWriter();
    try (PrintWriter pw = new PrintWriter(sw)) {
      Map<String, DeclReader> declFiles = new LinkedHashMap<>();
      declFiles.put("a", parse("a", a));
      declFiles.put("b", parse("b", b));
      MergeComparability.merge(declFiles, pw);
    }
    return Arrays.asList(sw.toString().split("\\R", -1));
  }

  /**
   * Returns the expected output of merging files "a" and "b".
   *
   * @param ppts the expected program point declarations
   * @return the lines of the expected output
   */
  @SafeVarargs
  private static List<String> expected(List<String>... ppts) {
    List<String> result = new ArrayList<>(MERGED_HEADER);
    for (List<String> ppt : ppts) {
      result.addAll(ppt);
    }
    // The output ends with a line separator, so splitting it yields a final empty string.
    result.add("");
    return result;
  }

  /** Comparabilities that differ across runs are unioned, transitively. */
  @Test
  public void testUnion() {
    // Run 1: {a, b}, {c}, {d}.  Run 2: {a}, {b, c}, {d}.  Merged: {a, b, c}, {d}.
    assertEquals(
        expected(ppt("1", "1", "1", "2")),
        merge(file(ppt("2", "2", "3", "4")), file(ppt("7", "5", "5", "6"))));
  }

  /** Identical inputs produce the same partition, renumbered. */
  @Test
  public void testIdentical() {
    assertEquals(
        expected(ppt("1", "2", "1", "3")),
        merge(file(ppt("5", "6", "5", "7")), file(ppt("5", "6", "5", "7"))));
  }

  /** Array index comparabilities are merged with scalar comparabilities. */
  @Test
  public void testArrays() {
    // Run 1: a's index is comparable to b.  Run 2: b is comparable to c.
    assertEquals(
        expected(ppt("1[2]", "2", "2")),
        merge(file(ppt("2[3]", "3", "4")), file(ppt("2[3]", "4", "4"))));
  }

  /** A variable that is comparable to everything in one run is comparable to everything. */
  @Test
  public void testNegative() {
    assertEquals(
        expected(ppt("-1", "1", "1")), merge(file(ppt("2", "2", "2")), file(ppt("-1", "3", "3"))));
  }

  /** A variable with no comparability record is comparable to everything. */
  @Test
  public void testMissingComparability() {
    assertEquals(expected(ppt(null, "1")), merge(file(ppt(null, "2")), file(ppt("2", "2"))));
  }

  /**
   * An array variable with no comparability record in some file is comparable to everything,
   * regardless of the order of the files.
   */
  @Test
  public void testMissingArrayComparability() {
    assertEquals(expected(ppt(null, "1")), merge(file(ppt(null, "2")), file(ppt("3[4]", "2"))));
    assertEquals(expected(ppt("-1", "1")), merge(file(ppt("3[4]", "2")), file(ppt(null, "2"))));
  }

  /**
   * A scalar negative comparability on an array variable is comparable to everything, so the output
   * of merging can itself be merged with DynComp output.
   */
  @Test
  public void testScalarNegativeArrayComparability() {
    assertEquals(expected(ppt("-1", "1")), merge(file(ppt("-1", "2")), file(ppt("3[4]", "2"))));
    assertEquals(expected(ppt("-1", "1")), merge(file(ppt("3[4]", "2")), file(ppt("-1", "2"))));
  }

  /** A negative component of an array comparability is negative in the output. */
  @Test
  public void testNegativeArrayComponent() {
    assertEquals(
        expected(ppt("1[-1]", "2")), merge(file(ppt("5[-1]", "3")), file(ppt("5[6]", "3"))));
  }

  /**
   * A chain of comparabilities does not pass through a variable that is comparable to everything.
   */
  @Test
  public void testNoChainThroughUniversal() {
    assertEquals(
        expected(ppt("1", "-1", "2")), merge(file(ppt("1", "1", "2")), file(ppt("1", "-1", "2"))));
  }

  /** A program point that appears in only one file is included in the output. */
  @Test
  public void testDisjointPpts() {
    assertEquals(
        expected(namedPpt("C.m():::ENTER", "1", "2"), namedPpt("C.n():::ENTER", "1", "1")),
        merge(
            file(namedPpt("C.m():::ENTER", "3", "4")), file(namedPpt("C.n():::ENTER", "5", "5"))));
  }

  /** Comparability values are local to a program point. */
  @Test
  public void testPerPpt() {
    assertEquals(
        expected(namedPpt("C.m():::ENTER", "1", "2"), namedPpt("C.n():::ENTER", "1", "2")),
        merge(
            file(namedPpt("C.m():::ENTER", "2", "3"), namedPpt("C.n():::ENTER", "2", "3")),
            file(namedPpt("C.m():::ENTER", "2", "3"), namedPpt("C.n():::ENTER", "3", "2"))));
  }

  /** Program points with different variables cannot be merged. */
  @Test
  public void testMismatchedVariables() {
    List<String> a = file(ppt("1", "1"));
    List<String> b = file(ppt("1", "1", "1"));
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
  }

  /** Program points whose variables have different types cannot be merged. */
  @Test
  public void testMismatchedTypes() {
    List<String> a = file(ppt("1", "1"));
    List<String> b = new ArrayList<>(file(ppt("1", "1")));
    b.set(b.lastIndexOf("  rep-type int"), "  rep-type double");
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
  }

  /** Types separated from their keyword by a tab are compared, not ignored. */
  @Test
  public void testMismatchedTabSeparatedTypes() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.set(a.lastIndexOf("  rep-type int"), "  rep-type\tint");
    List<String> b = new ArrayList<>(file(ppt("1", "1")));
    b.set(b.lastIndexOf("  rep-type int"), "  rep-type\tdouble");
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
    // Differences in whitespace alone do not prevent merging.
    merge(a, file(ppt("1", "1")));
  }

  /** Sample records, such as those in a .dtrace file, are rejected. */
  @Test
  public void testSampleRecord() {
    List<String> dtrace = new ArrayList<>(file(ppt("1")));
    dtrace.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "a", "3", "1", ""));
    assertThrows(Daikon.UserError.class, () -> parse("a", dtrace));
  }

  /** Comparability files without implicit comparability cannot be merged. */
  @Test
  public void testNoneComparability() {
    List<String> none = new ArrayList<>(file(ppt("1")));
    none.set(none.indexOf("var-comparability implicit"), "var-comparability none");
    assertThrows(Daikon.UserError.class, () -> merge(none, file(ppt("1"))));
  }

  /** A variable line whose name is separated from its keyword by a tab is a variable. */
  @Test
  public void testTabSeparatedVariable() {
    List<String> a = new ArrayList<>(file(ppt("2", "2", "3")));
    a.set(a.indexOf("variable a"), "variable\ta");
    List<String> expected = new ArrayList<>(expected(ppt("1", "1", "2")));
    expected.set(expected.indexOf("variable a"), "variable\ta");
    assertEquals(expected, merge(a, file(ppt("4", "4", "5"))));
  }

  /**
   * Header and ppt records whose tokens are separated by tabs or multiple spaces are recognized.
   */
  @Test
  public void testWhitespaceInHeaderAndPpt() {
    List<String> a = new ArrayList<>(file(ppt("2", "3")));
    a.set(a.indexOf("decl-version 2.0"), "decl-version\t2.0");
    a.set(a.indexOf("var-comparability implicit"), "var-comparability  implicit");
    a.set(a.indexOf("ppt C.m():::ENTER"), "ppt\tC.m():::ENTER");
    List<String> expected = new ArrayList<>(expected(ppt("1", "1")));
    expected.set(expected.indexOf("ppt C.m():::ENTER"), "ppt\tC.m():::ENTER");
    assertEquals(expected, merge(a, file(ppt("4", "4"))));
  }

  /** A file without a var-comparability record uses implicit comparability. */
  @Test
  public void testMissingVarComparability() {
    List<String> a = new ArrayList<>(file(ppt("2", "3")));
    a.remove("var-comparability implicit");
    merge(file(ppt("4", "4")), a);
    merge(a, file(ppt("4", "4")));
  }

  /** Program points whose variables have different flags cannot be merged. */
  @Test
  public void testMismatchedFlags() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.add(a.indexOf("variable b"), "  flags nomod");
    List<String> b = new ArrayList<>(file(ppt("1", "1")));
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
    // Differences in whitespace alone do not prevent merging.
    List<String> c = new ArrayList<>(file(ppt("1", "1")));
    c.add(c.indexOf("variable b"), "  flags\tnomod");
    merge(a, c);
  }

  /** The order of flags in a flags record does not prevent merging. */
  @Test
  public void testFlagsOrder() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.add(a.indexOf("variable b"), "  flags is_param nomod");
    List<String> b = new ArrayList<>(file(ppt("1", "1")));
    b.add(b.indexOf("variable b"), "  flags nomod is_param");
    merge(a, b);
  }

  /** Program points with different ppt-level records cannot be merged. */
  @Test
  public void testMismatchedPptRecords() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.add(a.indexOf("variable a"), "parent parent C:::OBJECT 1");
    assertThrows(Daikon.UserError.class, () -> merge(a, file(ppt("1", "1"))));
  }

  /** A program point declared twice in one file cannot be rewritten. */
  @Test
  public void testDuplicatePpt() {
    List<String> twice = file(ppt("1"), ppt("1"));
    assertThrows(Daikon.UserError.class, () -> parse("a", twice));
  }

  /** A DeclReader that is not for rewriting skips sample records and allows duplicate ppts. */
  @Test
  public void testNotForRewriting() {
    List<String> lines = new ArrayList<>(file(ppt("1", "2"), ppt("3", "3")));
    lines.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "a", "3", "1", ""));
    DeclReader reader = parse("a", lines, false);
    assertEquals(1, reader.ppts.size());
    DeclReader.DeclPpt ppt = castNonNull(reader.find_ppt("C.m():::ENTER"));
    DeclReader.DeclVarInfo var = castNonNull(ppt.find_var("a"));
    assertEquals("3", castNonNull(var.get_comparability()));
    assertEquals(Collections.emptyList(), ppt.declHeaderLines);
    assertEquals(Collections.emptyList(), var.lines);
  }

  /** A rewriting DeclReader rejects a variable declared twice and multiple comparabilities. */
  @Test
  public void testDuplicateVariableAndComparability() {
    List<String> twice = new ArrayList<>(file(ppt("1")));
    twice.addAll(twice.size() - 1, var("a", "2"));
    assertThrows(Daikon.UserError.class, () -> parse("a", twice));
    List<String> twoComparabilities = new ArrayList<>(file(ppt("1")));
    twoComparabilities.add(twoComparabilities.size() - 1, "  comparability 2");
    assertThrows(Daikon.UserError.class, () -> parse("a", twoComparabilities));
  }

  /**
   * A DeclReader that is not for rewriting allows a variable declared twice and multiple
   * comparabilities; the last one wins.
   */
  @Test
  public void testDuplicateVariableAndComparabilityNotForRewriting() {
    List<String> twice = new ArrayList<>(file(ppt("1")));
    twice.addAll(twice.size() - 1, var("a", "2"));
    twice.add(twice.size() - 1, "  comparability 3");
    DeclReader reader = parse("a", twice, false);
    DeclReader.DeclPpt ppt = castNonNull(reader.find_ppt("C.m():::ENTER"));
    DeclReader.DeclVarInfo var = castNonNull(ppt.find_var("a"));
    assertEquals("3", castNonNull(var.get_comparability()));
  }

  /** A DeclReader that is not for rewriting skips a sample record for a variable named "ppt". */
  @Test
  public void testSampleVariableNamedPpt() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "ppt", "3", "1", ""));
    DeclReader reader = parse("a", lines, false);
    assertEquals(1, reader.ppts.size());
  }

  /** A malformed comparability is reported as a user error. */
  @Test
  public void testMalformedComparability() {
    assertThrows(Daikon.UserError.class, () -> merge(file(ppt("1]", "1")), file(ppt("1", "1"))));
    assertThrows(Daikon.UserError.class, () -> merge(file(ppt("x", "1")), file(ppt("1", "1"))));
  }
}
