package daikon.test;

import junit.framework.*;
import java.util.Comparator;
import daikon.*;
import utilMDE.ArraysMDE;

public class ProglangTypeTest extends TestCase {

  public static void main(String[] args) {
    daikon.LogHelper.setupLogs (daikon.LogHelper.INFO);
    junit.textui.TestRunner.run(new TestSuite(ProglangTypeTest.class));
  }

  public ProglangTypeTest(String name) {
    super(name);
  }

  static Comparator<long[]> longarrcomparator = new ArraysMDE.LongArrayComparatorLexical();
  static Comparator<String[]> comparrcomparator = new ArraysMDE.ComparableArrayComparatorLexical<String>();

  // Runtime type of first argument is long[]
  boolean longarrcomp(Object a, long[] b) {
    return longarrcomparator.compare((long[]) a, b) == 0;
  }

  // Runtime type of first (and second) argument is Comparable[]
  boolean comparrcomp(Object a, Object[] b) {
    String[] a1 = (String[]) a;
    String[] b1 = (String[]) b;
    boolean result = comparrcomparator.compare(a1, b1) == 0;
    if (!result)
      System.out.println("Arrays differ: " + ArraysMDE.toString(a1)
                          + ", " + ArraysMDE.toString(b));
    return result;
  }

  public void test_parse_value() {
    ProglangType pint = ProglangType.INT;
    assert pint.parse_value("1").equals(new Long(1));
    assert pint.parse_value("0").equals(new Long(0));
    assert pint.parse_value("-3").equals(new Long(-3));

    ProglangType pstring = ProglangType.STRING;
    assert pstring.parse_value("\"foo\"").equals("foo");
    assert pstring.parse_value("\"\"").equals("");
    assert pstring.parse_value("\"\"foo\"\"").equals("\"foo\"");
    assert pstring.parse_value("\"foo bar\"").equals("foo bar");
    assert pstring.parse_value("null") == null;

    ProglangType pinta = ProglangType.INT_ARRAY;
    assert longarrcomp(pinta.parse_value("[]"), new long[] {});
    assert longarrcomp(pinta.parse_value("[1]"), new long[] { 1 });
    assert longarrcomp(pinta.parse_value("[-2]"), new long[] { -2 });
    assert longarrcomp(pinta.parse_value("[1 2 3]"), new long[] { 1, 2, 3 });

    ProglangType pstringa = ProglangType.STRING_ARRAY;
    assert comparrcomp(pstringa.parse_value("[]"), new String[] {});
    assert comparrcomp(pstringa.parse_value("[\"foo\"]"), new String[] { "foo" });
    assert comparrcomp(pstringa.parse_value("[\"f\\\"oo\"]"), new String[] { "f\"oo" });
    assert comparrcomp(pstringa.parse_value("[\"f\\noo\"]"), new String[] { "f\noo" });
    assert comparrcomp(pstringa.parse_value("[\"foo\" \"bar\"]"), new String[] { "foo", "bar" });
    assert comparrcomp(pstringa.parse_value("[\"foo bar\" \"baz\"]"), new String[] { "foo bar", "baz" });
    assert comparrcomp(pstringa.parse_value("[\"foo\" null \"baz\"]"), new String[] { "foo", null, "baz" });
  }

}
