package daikon.test.split;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import daikon.Daikon;
import daikon.FileIO;
import daikon.PptMap;
import daikon.PptTopLevel;
import daikon.ValueTuple;
import daikon.VarInfo;
import daikon.split.SpinfoFile;
import daikon.split.Splitter;
import daikon.split.SplitterFactory;
import daikon.split.SplitterList;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/**
 * Compiles, loads, and evaluates the splitters for the conditions in QuantCalls.spinfo, which call
 * daikon.Quant methods. SplitterFactoryTest only compares the generated source code with goal
 * files.
 */
public final class QuantCallsSplitterTest {

  /** The directory that contains the test inputs. */
  private static final String targetDir = "daikon/test/split/targets/";

  /** The name of the program point to which the splitters apply. */
  private static final String pptName = "misc.QuantCalls.m";

  /** The conditions that cannot be translated, because they apply a method to an array. */
  private static final String[] untranslatableConditions = {
    "daikon.Quant.getElement_boolean(this.a, 0)",
    "java.util.Arrays.equals(this.a, this.b)",
    "daikon.Quant.size(this.h) > daikon.Quant.getElement_int(this.d, 0)"
  };

  /** Maps each translatable condition to its value for the values set by {@link #makeVt}. */
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
  }

  /**
   * Checks that the untranslatable conditions are reported, and that the splitters for the other
   * conditions compile and evaluate to the expected values.
   *
   * @throws IOException if a test input cannot be read
   */
  @Test
  @SuppressWarnings("nullness:contracts.precondition") // parse_spinfofile sets the temp directory
  public void testQuantCalls() throws IOException {
    SpinfoFile spinfo = SplitterFactory.parse_spinfofile(new File(targetDir + "QuantCalls.spinfo"));
    PptMap ppts = new PptMap();
    // Other tests may have set the patterns that filter the program points and variables.
    Pattern oldPptRegexp = Daikon.ppt_regexp;
    Pattern oldPptOmitRegexp = Daikon.ppt_omit_regexp;
    Pattern oldVarRegexp = Daikon.var_regexp;
    Pattern oldVarOmitRegexp = Daikon.var_omit_regexp;
    Daikon.ppt_regexp = null;
    Daikon.ppt_omit_regexp = null;
    Daikon.var_regexp = null;
    Daikon.var_omit_regexp = null;
    try {
      FileIO.read_data_trace_file(targetDir + "QuantCalls.decls", ppts);
    } finally {
      Daikon.ppt_regexp = oldPptRegexp;
      Daikon.ppt_omit_regexp = oldPptOmitRegexp;
      Daikon.var_regexp = oldVarRegexp;
      Daikon.var_omit_regexp = oldVarOmitRegexp;
    }
    PptTopLevel ppt = ppts.get("misc.QuantCalls.m(int):::EXIT10");
    if (ppt == null) {
      throw new AssertionError("No program point in " + ppts.nameStringSet());
    }

    PrintStream oldOut = System.out;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));
    try {
      SplitterFactory.load_splitters(ppt, Collections.singletonList(spinfo));
    } finally {
      System.setOut(oldOut);
    }
    String output = new String(out.toByteArray(), StandardCharsets.UTF_8);

    for (String condition : untranslatableConditions) {
      assertTrue(output, output.contains(condition + " cannot be parsed or translated:"));
    }
    assertTrue(output, output.contains("the splitter cannot pass the array this.a to a method"));
    assertTrue(
        output,
        output.contains(
            "Cannot translate daikon.Quant.size(this_h): the first argument is not an array whose"
                + " elements are available to the splitter"));
    assertTrue(
        output,
        output.contains(
            "Cannot translate daikon.Quant.getElement_int(this_d,0): the element type of this.d[..]"
                + " is not the one that the method expects"));

    Splitter[] factories = SplitterList.get_raw(pptName);
    if (factories == null) {
      throw new AssertionError(output);
    }
    assertEquals(output, expectedValues.size(), factories.length);
    ValueTuple vt = makeVt(ppt);
    for (Splitter factory : factories) {
      String condition = factory.condition();
      Boolean expected = expectedValues.get(condition);
      assertNotNull(condition, expected);
      Splitter splitter = factory.instantiateSplitter(ppt);
      assertTrue(condition, splitter.valid());
      assertEquals(condition, expected, splitter.test(vt));
    }
  }

  /**
   * Returns a ValueTuple that gives values to the variables of ppt that the conditions use.
   *
   * @param ppt the program point to which the splitters apply
   * @return a ValueTuple for ppt
   */
  private static ValueTuple makeVt(PptTopLevel ppt) {
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

    int size = 0;
    for (VarInfo vi : ppt.var_infos) {
      size = Math.max(size, vi.value_index + 1);
    }
    @Nullable Object[] vals = new @Nullable Object[size];
    int[] mods = new int[size];
    Arrays.fill(mods, ValueTuple.MODIFIED);
    for (VarInfo vi : ppt.var_infos) {
      if (vi.value_index >= 0) {
        vals[vi.value_index] = values.get(vi.name());
      }
    }
    return ValueTuple.makeUninterned(vals, mods);
  }
}
