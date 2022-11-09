package daikon.test;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;

import daikon.FileIO;
import daikon.ProglangType;
import java.util.Arrays;
import java.util.Comparator;
import org.junit.BeforeClass;
import org.junit.Test;
import org.plumelib.util.ArraysPlume;

/** Test the ProglangType class. */
@SuppressWarnings("nullness") // testing code
public class ProglangTypeTest {

  /** Creates a ProglangTypeTest. */
  public ProglangTypeTest() {}

  /** Comparator for arrays of longs. */
  static Comparator<long[]> longarrcomparator = ArraysPlume.LongArrayComparatorLexical.it;

  /** Comparator for arrays of Strings. */
  static Comparator<String[]> comparrcomparator =
      new ArraysPlume.ComparableArrayComparatorLexical<String>();

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  /**
   * Asserts that the two arrays are equal.
   *
   * @param a the first array; its run-time is long[]
   * @param b the second array
   */
  void longarrEquals(Object a, long[] b) {
    assertEquals(0, longarrcomparator.compare((long[]) a, b));
  }

  /**
   * Asserts that the two arrays are equal.
   *
   * @param a the first array; its run-time is Comparable[]
   * @param b the second array; its run-time is Comparable[]
   */
  void comparrEquals(Object a, Object[] b) {
    String[] a1 = (String[]) a;
    String[] b1 = (String[]) b;
    assertEquals(
        "Arrays differ: " + Arrays.toString(a1) + ", " + Arrays.toString(b),
        0,
        comparrcomparator.compare(a1, b1));
  }

  // a helper for parse_value
  private Object parse_value_helper(ProglangType pt, String s) {
    return pt.parse_value(s, null, "test_parse_value_helper");
  }

  // a helper for test_parse_value
  private void test_parse_value_helper(ProglangType pt, String s, Object value) {
    Object result = parse_value_helper(pt, s);
    assertEquals(
        String.format("test_parse_value_helper(%s, %s, %s) => %s", pt, s, value, result),
        value,
        result);
  }

  @Test
  public void test_parse_value() {
    ProglangType pint = ProglangType.INT;
    test_parse_value_helper(pint, "1", Long.valueOf(1));
    test_parse_value_helper(pint, "0", Long.valueOf(0));
    test_parse_value_helper(pint, "-3", Long.valueOf(-3));

    ProglangType pstring = ProglangType.STRING;
    test_parse_value_helper(pstring, "\"foo\"", "foo");
    test_parse_value_helper(pstring, "\"\"", "");
    test_parse_value_helper(pstring, "\"\"foo\"\"", "\"foo\"");
    test_parse_value_helper(pstring, "\"foo bar\"", "foo bar");
    test_parse_value_helper(pstring, "null", null);

    ProglangType pinta = ProglangType.INT_ARRAY;
    longarrEquals(parse_value_helper(pinta, "[]"), new long[] {});
    longarrEquals(parse_value_helper(pinta, "[1]"), new long[] {1});
    longarrEquals(parse_value_helper(pinta, "[-2]"), new long[] {-2});
    longarrEquals(parse_value_helper(pinta, "[1 2 3]"), new long[] {1, 2, 3});

    ProglangType pstringa = ProglangType.STRING_ARRAY;
    comparrEquals(parse_value_helper(pstringa, "[]"), new String[] {});
    comparrEquals(parse_value_helper(pstringa, "[\"foo\"]"), new String[] {"foo"});
    comparrEquals(parse_value_helper(pstringa, "[\"f\\\"oo\"]"), new String[] {"f\"oo"});
    comparrEquals(parse_value_helper(pstringa, "[\"f\\noo\"]"), new String[] {"f\noo"});
    comparrEquals(parse_value_helper(pstringa, "[\"foo\" \"bar\"]"), new String[] {"foo", "bar"});
    comparrEquals(
        parse_value_helper(pstringa, "[\"foo bar\" \"baz\"]"), new String[] {"foo bar", "baz"});
    comparrEquals(
        parse_value_helper(pstringa, "[\"foo\" null \"baz\"]"), new String[] {"foo", null, "baz"});
  }
}
