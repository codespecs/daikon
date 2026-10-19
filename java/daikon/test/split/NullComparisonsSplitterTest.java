package daikon.test.split;

import static org.junit.Assert.assertEquals;

import daikon.PptTopLevel;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/**
 * Compiles, loads, and evaluates the splitters for the conditions in NullComparisons.spinfo, which
 * compare with null both variables that the splitter represents by a hashcode and variables that it
 * represents as a reference.
 */
public final class NullComparisonsSplitterTest {

  /** Maps each condition to its value for the values set by {@link #testNullComparisons}. */
  private static final Map<String, Boolean> expectedValues = new HashMap<>();

  static {
    expectedValues.put("this.obj == null", true);
    expectedValues.put("this.objs[1] != null", true);
    expectedValues.put("this.sign == null", false);
    expectedValues.put("null != this.sign", true);
    expectedValues.put("(this.sign) != null && this.obj == null", true);
    expectedValues.put("this.sign != null && (this.objs[0]) == null", true);
    expectedValues.put("this.s[0] == null", true);
    expectedValues.put("this.s[1] == null", false);
  }

  /**
   * Checks that the splitters compile and evaluate to the expected values.
   *
   * @throws IOException if a test input cannot be read
   */
  @Test
  public void testNullComparisons() throws IOException {
    PptTopLevel ppt =
        SplitterTestUtils.readPpt("NullComparisons.decls", "misc.NullComparisons.m():::EXIT10");
    Map<String, Object> values = new HashMap<>();
    values.put("this.sign", "+");
    // A hashcode of 0 represents null.
    values.put("this.obj", 0L);
    values.put("this.s[..]", new @Nullable String[] {null, "y"});
    values.put("this.objs[..]", new long[] {0, 7});
    SplitterTestUtils.LoadedSplitters loaded =
        SplitterTestUtils.loadAndTest(
            "NullComparisons.spinfo", ppt, "misc.NullComparisons.m", values);
    assertEquals(loaded.output, expectedValues, loaded.values);
  }
}
