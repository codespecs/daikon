package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import daikon.PptTopLevel;
import daikon.split.PptSplitter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;

/**
 * Compiles, loads, and evaluates the splitters for the conditions in QuantCalls.spinfo, which call
 * daikon.Quant methods. SplitterFactoryTest only compares the generated source code with goal
 * files.
 */
public final class QuantCallsSplitterTest {

  /** The name of the program point to which the splitters apply. */
  private static final String pptName = "misc.QuantCalls.m";

  /**
   * The conditions that cannot be translated, because they apply a daikon.Quant method to a
   * variable whose elements are not available to the splitter or have the wrong type.
   */
  private static final String[] untranslatableConditions = {
    "daikon.Quant.getElement_boolean(this.a, 0)",
    "daikon.Quant.size(this.h) > daikon.Quant.getElement_int(this.d, 0)"
  };

  /** Maps each translatable condition to its value for the values in {@link #testQuantCalls}. */
  private static final Map<String, Boolean> expectedValues = new HashMap<>();

  static {
    expectedValues.put(
        "daikon.Quant.getElement_int(this.a, daikon.Quant.getElement_int(this.b, i)) > 0", true);
    expectedValues.put("daikon.Quant.size(this.a) == i", false);
    expectedValues.put("daikon.Quant.fuzzy.eq(this.x, 0.0)", false);
    expectedValues.put("isEmpty()", false);
    expectedValues.put("daikon.Quant.getElement_boolean(this.flags, i)", true);
    expectedValues.put(
        "daikon.Quant.getElement_double(this.d, Math.max(i, 0)) > java.lang.Math.abs(this.x)",
        false);
    expectedValues.put("\"x\".equals(daikon.Quant.getElement_String(this.s, 0))", true);
    expectedValues.put(
        "daikon.Quant.getElement_char(this.str, 0) == 'a' && daikon.Quant.size(this.str) > 1",
        true);
    expectedValues.put("daikon.Quant.getElement_String(this.s, 0) == null", false);
    expectedValues.put("daikon.Quant.getElement_Object(this.s, i) != null", true);
    expectedValues.put(
        "this.a[this.sh[0]] > 0 && daikon.Quant.getElement_short(this.sh, 1) > 0", true);
    expectedValues.put("this.sign == null", false);
    expectedValues.put(
        "this.sign != null && (daikon.Quant.getElement_String(this.s, 0)) == null", false);
    expectedValues.put("daikon.Quant.getElement_String(this.s, 0).trim() != null", true);
    expectedValues.put("this.obj == null", true);
    expectedValues.put(
        "daikon.Quant.getElement_Object(this.objs, i) != null && this.objs[0] == null", true);
    expectedValues.put("java.util.Arrays.equals(this.a, this.b)", false);
  }

  /**
   * Checks that the untranslatable conditions are reported, and that the splitters for the other
   * conditions compile and evaluate to the expected values.
   *
   * @throws IOException if a test input cannot be read
   */
  @Test
  public void testQuantCalls() throws IOException {
    PptTopLevel ppt =
        SplitterTestUtils.readPpt("QuantCalls.decls", "misc.QuantCalls.m(int):::EXIT10");
    Map<String, Object> values = new HashMap<>();
    values.put("i", 1L);
    values.put("this.a[..]", new long[] {5, -3, 7});
    values.put("this.b[..]", new long[] {2, 0});
    values.put("this.x", 0.5);
    values.put("this.flags[..]", new long[] {0, 1});
    values.put("this.d[..]", new double[] {1.0, 0.25});
    values.put("this.s[..]", new String[] {"x", " y "});
    values.put("this.str", "ab");
    values.put("this.sh[..]", new long[] {2, 4});
    values.put("this.obj", 0L);
    values.put("this.objs[..]", new long[] {0, 7});
    values.put("this.sign", "+");
    // Report the untranslatable conditions, which this test checks for.
    boolean oldSuppress = PptSplitter.dkconfig_suppressSplitterErrors;
    PptSplitter.dkconfig_suppressSplitterErrors = false;
    SplitterTestUtils.LoadedSplitters loaded;
    try {
      loaded = SplitterTestUtils.load("QuantCalls.spinfo", ppt, pptName);
    } finally {
      PptSplitter.dkconfig_suppressSplitterErrors = oldSuppress;
    }
    String output = loaded.output;

    for (String condition : untranslatableConditions) {
      assertTrue(output, output.contains(condition + " cannot be parsed or translated:"));
    }
    assertTrue(
        output,
        output.contains(
            "Cannot translate daikon.Quant.size(this_h): the first argument is not an array whose"
                + " elements are available to the splitter"));
    assertTrue(
        output,
        output.contains(
            "Cannot translate daikon.Quant.getElement_int(this_d,0): the element type of"
                + " this.d[..] is not the one that the method expects"));
    assertEquals(output, expectedValues, loaded.test(values));
  }
}
