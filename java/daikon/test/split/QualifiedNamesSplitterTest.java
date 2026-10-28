package daikon.test.split;

import static org.junit.Assert.assertEquals;

import daikon.PptTopLevel;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

/**
 * Compiles, loads, and evaluates the splitters for the conditions in QualifiedNames.spinfo, which
 * contain names that do not start with a variable name, such as "java.lang.Math.abs", in expression
 * and type contexts.
 */
public final class QualifiedNamesSplitterTest {

  /** Maps each condition to its value for the values set by {@link #testQualifiedNames}. */
  private static final Map<String, Boolean> expectedValues = new HashMap<>();

  static {
    expectedValues.put("this.x < 0", true);
    expectedValues.put("p.count > 2", true);
    expectedValues.put("java.lang.Math.abs(this.x) > 1", true);
    expectedValues.put("java.lang.Math.abs(this.x) > 3", false);
    expectedValues.put("daikon.Quant.fuzzy.eq(this.x, -2.5)", true);
    expectedValues.put("Integer.MAX_VALUE > p.count", true);
    expectedValues.put("orig(p.count) > 2", false);
    expectedValues.put("java.lang.Integer.MAX_VALUE > orig(p.count)", true);
    expectedValues.put(
        "((java.lang.Number) java.lang.Double.valueOf(this.x)).intValue() == -2", true);
    expectedValues.put("java.lang.String.class.getName().length() > p.count", true);
    expectedValues.put("orig(java.lang.Math.abs(p.count)) > 0", true);
    expectedValues.put("orig(Integer.MAX_VALUE) > p.count", true);
    expectedValues.put("orig(java.lang.Integer.MAX_VALUE) > p.count", true);
    expectedValues.put("orig(java.lang.Math.abs(p.count) + p.count) > 2", false);
  }

  /**
   * Checks that the splitters compile and evaluate to the expected values.
   *
   * @throws IOException if a test input cannot be read
   */
  @Test
  public void testQualifiedNames() throws IOException {
    PptTopLevel ppt =
        SplitterTestUtils.readPpt(
            "QualifiedNames.decls", "misc.QualifiedNames.m(misc.QualifiedNames):::EXIT10");
    Map<String, Object> values = new HashMap<>();
    values.put("this.x", -2.5);
    values.put("p.count", 3L);
    values.put("orig(p.count)", 1L);
    values.put("java.lang.Integer.MAX_VALUE", (long) Integer.MAX_VALUE);
    values.put("orig(java.lang.Integer.MAX_VALUE)", (long) Integer.MAX_VALUE);
    SplitterTestUtils.LoadedSplitters loaded = SplitterTestUtils.load("QualifiedNames.spinfo", ppt);
    assertEquals(loaded.output, expectedValues, loaded.test(values));
  }
}
