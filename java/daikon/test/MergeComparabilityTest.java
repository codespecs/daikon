package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import daikon.Daikon;
import daikon.tools.MergeComparability;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
      result.addAll(var(String.valueOf((char) ('a' + i)), comparabilities[i]));
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
   * Merges two declaration files, named "a" and "b".
   *
   * @param a the lines of the first file
   * @param b the lines of the second file
   * @return the lines of the merged output
   */
  private static List<String> merge(List<String> a, List<String> b) {
    StringWriter sw = new StringWriter();
    try (PrintWriter pw = new PrintWriter(sw)) {
      MergeComparability.merge(
          Arrays.asList(
              MergeComparability.parseDeclFile("a", a), MergeComparability.parseDeclFile("b", b)),
          pw);
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
    assertThrows(
        Daikon.UserError.class, () -> merge(file(ppt("1", "1")), file(ppt("1", "1", "1"))));
  }

  /** Sample records, such as those in a .dtrace file, are rejected. */
  @Test
  public void testSampleRecord() {
    List<String> dtrace = new ArrayList<>(file(ppt("1")));
    dtrace.addAll(Arrays.asList("C.m():::ENTER", "this_invocation_nonce", "0", "a", "3", "1", ""));
    assertThrows(Daikon.UserError.class, () -> MergeComparability.parseDeclFile("a", dtrace));
  }

  /** Comparability files without implicit comparability cannot be merged. */
  @Test
  public void testNoneComparability() {
    List<String> none = new ArrayList<>(file(ppt("1")));
    none.set(none.indexOf("var-comparability implicit"), "var-comparability none");
    assertThrows(Daikon.UserError.class, () -> MergeComparability.parseDeclFile("a", none));
  }
}
