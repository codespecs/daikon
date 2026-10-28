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

  /**
   * Checks that the splitters compile and evaluate to the expected values.
   *
   * @throws IOException if a test input cannot be read
   */
  @Test
  public void testNullComparisons() throws IOException {
    PptTopLevel ppt =
        SplitterTestUtils.readPpt("NullComparisons.decls", "misc.NullComparisons.m():::EXIT10");
    SplitterTestUtils.LoadedSplitters loaded =
        SplitterTestUtils.load("NullComparisons.spinfo", ppt);

    // A hashcode of 0 represents null.  Daikon treats a null String as a missing value, so a
    // splitter is never evaluated on a null this.sign.
    Map<String, Object> values = new HashMap<>();
    values.put("this.sign", "+");
    values.put("this.obj", 0L);
    values.put("this.s[..]", new @Nullable String[] {null, "y"});
    values.put("this.objs", 5L);
    values.put("this.objs[..]", new long[] {0, 7});
    Map<String, Boolean> expected = new HashMap<>();
    expected.put("this.obj == null", true);
    expected.put("this.objs[1] != null", true);
    expected.put("this.sign == null", false);
    expected.put("null != this.sign", true);
    expected.put("(this.sign) != null && this.obj == null", true);
    expected.put("this.sign != null && (this.objs[0]) == null", true);
    expected.put("this.s[0] == null", true);
    expected.put("this.s[1] == null", false);
    expected.put("this.objs == null", false);
    expected.put("this.sign == (null)", false);
    expected.put("this.sign.trim() != null", true);
    expected.put("null == (this.s[1]).trim()", false);
    expected.put("String.valueOf(this.s[0]).equals(String.valueOf((Object) null))", true);
    expected.put("String.valueOf(this.s[0]) != null", true);
    expected.put("this.sign.equals(null)", false);
    expected.put("((String) this.sign) == null", false);
    expected.put("(this.sign != null ? this.obj : this.objs) == null", true);
    expected.put("this.obj == (this.objs == null ? null : this.obj)", true);
    expected.put("(this.sign != null ? null : this.obj) == null", true);
    assertEquals(loaded.output, expected, loaded.test(values));

    values.put("this.obj", 3L);
    values.put("this.objs", 0L);
    expected.put("this.obj == null", false);
    expected.put("(this.sign) != null && this.obj == null", false);
    expected.put("this.objs == null", true);
    expected.put("(this.sign != null ? this.obj : this.objs) == null", false);
    expected.put("this.obj == (this.objs == null ? null : this.obj)", false);
    assertEquals(loaded.output, expected, loaded.test(values));
  }
}
