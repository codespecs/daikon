package daikon.test;

import static daikon.tools.nullness.NullnessUtil.castNonNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.chicory.DeclReader;
import daikon.tools.MergeComparability;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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

  /**
   * A variable with no comparability record is comparable to everything, and the output gives it a
   * comparability record.
   */
  @Test
  public void testMissingComparability() {
    assertEquals(expected(ppt("-1", "1")), merge(file(ppt(null, "2")), file(ppt("2", "2"))));
  }

  /**
   * Distinct negative comparabilities remain distinct, because Daikon puts variables in the same
   * equality set only if their comparabilities are equal.
   */
  @Test
  public void testDistinctNegatives() {
    assertEquals(
        expected(ppt("-1", "-2", "-1", "1[-2]")),
        merge(file(ppt("-2", "-3", "-2", "4[-3]")), file(ppt("-2", "-3", "-2", "4[-3]"))));
    // Negative comparabilities that are equal in some file are equal in the output.
    assertEquals(
        expected(ppt("-1", "-1", "-1")),
        merge(file(ppt("-2", "-2", "-3")), file(ppt("-2", "-3", "-3"))));
    // Daikon treats a variable without a comparability record as having comparability -3.
    assertEquals(
        expected(ppt("-1", "-1", "-2")),
        merge(file(ppt(null, "-3", "-1")), file(ppt(null, "-3", "-1"))));
  }

  /**
   * An array variable with no comparability record in some file is comparable to everything,
   * regardless of the order of the files.
   */
  @Test
  public void testMissingArrayComparability() {
    assertEquals(expected(ppt("-1", "1")), merge(file(ppt(null, "2")), file(ppt("3[4]", "2"))));
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

  /**
   * Comparability is made consistent across the entry, exit, OBJECT, and CLASS program points of a
   * class, as DynComp does.
   */
  @Test
  public void testHierarchy() {
    // In file a, x and z are comparable at C.m():::EXIT1; in file b, y and z are.
    List<String> a =
        file(
            namedPpt("C:::CLASS", "1", "2"),
            namedPpt("C:::OBJECT", "1", "2"),
            namedPpt("C.m():::ENTER", "1", "2"),
            namedPpt("C.m():::EXIT1", "1", "2", "1"),
            namedPpt("C.m():::EXIT2", "1", "2", "1"),
            namedPpt("C.n():::ENTER", "1", "2"),
            namedPpt("D.m():::ENTER", "1", "2"));
    List<String> b =
        file(
            namedPpt("C:::CLASS", "1", "2"),
            namedPpt("C:::OBJECT", "1", "2"),
            namedPpt("C.m():::ENTER", "1", "2"),
            namedPpt("C.m():::EXIT1", "1", "2", "2"),
            namedPpt("C.m():::EXIT2", "1", "2", "2"),
            namedPpt("C.n():::ENTER", "1", "2"),
            namedPpt("D.m():::ENTER", "1", "2"));
    assertEquals(
        expected(
            namedPpt("C:::CLASS", "1", "1"),
            namedPpt("C:::OBJECT", "1", "1"),
            namedPpt("C.m():::ENTER", "1", "1"),
            namedPpt("C.m():::EXIT1", "1", "1", "1"),
            namedPpt("C.m():::EXIT2", "1", "1", "1"),
            // Comparability flows to an entry point only from the exit points of its method.
            namedPpt("C.n():::ENTER", "1", "2"),
            namedPpt("D.m():::ENTER", "1", "2")),
        merge(a, b));
  }

  /** Comparability does not flow from a CLASS program point to the OBJECT program point. */
  @Test
  public void testHierarchyClassToObject() {
    List<String> a = file(namedPpt("C:::CLASS", "1", "1"), namedPpt("C:::OBJECT", "1", "2"));
    assertEquals(
        expected(namedPpt("C:::CLASS", "1", "1"), namedPpt("C:::OBJECT", "1", "2")), merge(a, a));
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
    List<String> b = file(ppt("1"));
    assertThrows(Daikon.UserError.class, () -> merge(none, b));
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

  /** Whitespace within a string literal is significant when comparing records. */
  @Test
  public void testWhitespaceInStringLiteral() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.add(a.indexOf("variable b"), "  constant \"x  y\"");
    List<String> b = new ArrayList<>(file(ppt("1", "1")));
    b.add(b.indexOf("variable b"), "  constant \"x y\"");
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
    // Whitespace outside a string literal is not significant, and an escaped quote does not end a
    // string literal.
    List<String> c = new ArrayList<>(file(ppt("1", "1")));
    c.add(c.indexOf("variable b"), "  constant\t\"x  y\"");
    merge(a, c);
    assertEquals(
        Arrays.asList("constant", "\"a\\\" b\"", "c"),
        MergeComparability.tokenizeRespectingQuotes(" constant \"a\\\" b\"  c "));
  }

  /** Program points with different ppt-level records cannot be merged. */
  @Test
  public void testMismatchedPptRecords() {
    List<String> a = new ArrayList<>(file(ppt("1", "1")));
    a.add(a.indexOf("variable a"), "parent parent C:::OBJECT 1");
    List<String> b = file(ppt("1", "1"));
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
  }

  /** A program point declared twice in one file cannot be rewritten. */
  @Test
  public void testDuplicatePpt() {
    List<String> twice = file(ppt("1"), ppt("1"));
    assertThrows(Daikon.UserError.class, () -> parse("a", twice));
  }

  /** A DeclReader that is not for rewriting does not retain the text of declarations. */
  @Test
  public void testNotForRewriting() {
    DeclReader reader = parse("a", file(ppt("1")), false);
    DeclReader.DeclPpt ppt = castNonNull(reader.find_ppt("C.m():::ENTER"));
    DeclReader.DeclVarInfo var = castNonNull(ppt.find_var("a"));
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
   * A rewriting DeclReader rejects extra tokens and a ppt name without ":::". (A DeclReader that is
   * not for rewriting accepts them; see {@code DeclReaderTest.testLenient}.)
   */
  @Test
  public void testExtraTokens() {
    List<String> extraComparabilityToken = new ArrayList<>(file(ppt("1")));
    extraComparabilityToken.set(
        extraComparabilityToken.indexOf("  comparability 1"), "  comparability 1 x");
    List<String> extraVariableToken = new ArrayList<>(file(ppt("1")));
    extraVariableToken.set(extraVariableToken.indexOf("variable a"), "variable a x");
    List<String> noColons = new ArrayList<>(file(ppt("1")));
    noColons.set(noColons.indexOf("ppt C.m():::ENTER"), "ppt C.m()");
    for (List<String> lines :
        Arrays.asList(extraComparabilityToken, extraVariableToken, noColons)) {
      assertThrows(Daikon.UserError.class, () -> parse("a", lines));
    }
  }

  /** Records that DeclReader does not recognize are copied to the output. */
  @Test
  public void testUnrecognizedRecords() {
    List<String> a = new ArrayList<>(file(ppt("2", "2")));
    a.add(a.indexOf("variable a"), "new-ppt-record x");
    a.add(a.indexOf("variable b"), "  new-var-record y");
    List<String> expected = new ArrayList<>(expected(ppt("1", "1")));
    expected.add(expected.indexOf("variable a"), "new-ppt-record x");
    expected.add(expected.indexOf("variable b"), "  new-var-record y");
    assertEquals(expected, merge(a, a));
  }

  /** A ListImplementors record is copied to the output, and it must be the same in every file. */
  @Test
  public void testListImplementors() {
    List<String> a = new ArrayList<>(file(ppt("2", "2")));
    a.addAll(a.indexOf("ppt C.m():::ENTER"), Arrays.asList("ListImplementors", "p.MyList", ""));
    List<String> expected = new ArrayList<>(expected(ppt("1", "1")));
    expected.addAll(
        expected.indexOf("var-comparability implicit") + 1,
        Arrays.asList("ListImplementors", "p.MyList"));
    assertEquals(expected, merge(a, a));
    List<String> b = file(ppt("2", "2"));
    assertThrows(Daikon.UserError.class, () -> merge(a, b));
  }

  /** A program point declaration that is not ended by a blank line is rejected. */
  @Test
  public void testMissingBlankLineBetweenPpts() {
    List<String> lines = new ArrayList<>(HEADER);
    lines.addAll(namedPpt("C.m():::ENTER"));
    lines.remove(lines.size() - 1);
    lines.addAll(namedPpt("C.m():::EXIT1", "1"));
    assertThrows(Daikon.UserError.class, () -> parse("a", lines));
  }

  /** A "ppt" record within a variable declaration is rejected. */
  @Test
  public void testPptRecordInVariable() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.remove(lines.size() - 1);
    lines.addAll(namedPpt("C.m():::EXIT1", "1"));
    assertThrows(Daikon.UserError.class, () -> parse("a", lines));
  }

  /** A file without a decl-version record, such as an empty file, cannot be rewritten. */
  @Test
  public void testNoDeclVersion() {
    assertThrows(Daikon.UserError.class, () -> parse("a", Collections.emptyList()));
    assertThrows(Daikon.UserError.class, () -> parse("a", Arrays.asList("// comment", "")));
  }

  /** A header record that appears more than once cannot be rewritten. */
  @Test
  public void testDuplicateHeaderRecord() {
    List<String> lines = new ArrayList<>(file(ppt("1")));
    lines.add("var-comparability none");
    assertThrows(Daikon.UserError.class, () -> parse("a", lines));
  }

  /** An inconsistency in the input files leaves an existing output file unchanged. */
  @Test
  public void testOutputFile() throws IOException {
    Path dir = Files.createTempDirectory("MergeComparabilityTest");
    try {
      Path a = dir.resolve("a.decls-DynComp");
      Files.write(a, file(ppt("2", "2")));
      Path b = dir.resolve("b.decls-DynComp");
      Files.write(b, file(ppt("2", "2", "2")));
      Path output = dir.resolve("merged.decls-DynComp");
      MergeComparability.mainHelper(new String[] {"-o", output.toString(), a.toString()});
      List<String> merged = Files.readAllLines(output);
      assertTrue(merged.contains("  comparability 1"));
      assertThrows(
          Daikon.UserError.class,
          () ->
              MergeComparability.mainHelper(
                  new String[] {"-o", output.toString(), a.toString(), b.toString()}));
      assertEquals(merged, Files.readAllLines(output));
    } finally {
      try (Stream<Path> paths = Files.walk(dir)) {
        for (Path p : paths.sorted(Comparator.reverseOrder()).collect(Collectors.toList())) {
          Files.delete(p);
        }
      }
    }
  }

  /** A malformed comparability is reported as a user error. */
  @Test
  public void testMalformedComparability() {
    List<String> b = file(ppt("1", "1"));
    List<String> bracket = file(ppt("1]", "1"));
    assertThrows(Daikon.UserError.class, () -> merge(bracket, b));
    List<String> nonNumeric = file(ppt("x", "1"));
    assertThrows(Daikon.UserError.class, () -> merge(nonNumeric, b));
  }
}
