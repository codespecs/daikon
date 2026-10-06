package daikon.split;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/** Tests {@link SplitterJavaSource}. */
public class SplitterJavaSourceTest {

  /** Tests {@link SplitterJavaSource#replaceQuantArrayCalls}. */
  @Test
  public void testReplaceQuantArrayCalls() {
    assertEquals("x == 1", SplitterJavaSource.replaceQuantArrayCalls("x == 1"));
    assertEquals(
        "(this.a[i]) != null",
        SplitterJavaSource.replaceQuantArrayCalls(
            "daikon.Quant.getElement_Object(this.a, i) != null"));
    assertEquals(
        "(this.a.length)-1 != 0",
        SplitterJavaSource.replaceQuantArrayCalls("daikon.Quant.size(this.a)-1 != 0"));
    // Nested calls, and arguments that contain parentheses and commas.
    assertEquals(
        "(a[(b[f(x, y)])]) == (a.length)",
        SplitterJavaSource.replaceQuantArrayCalls(
            "daikon.Quant.getElement_int(a, daikon.Quant.getElement_int(b, f(x, y)))"
                + " == daikon.Quant.size(a)"));
    // Other daikon.Quant methods are left alone.
    assertEquals(
        "daikon.Quant.fuzzy.eq(x, 0.0)",
        SplitterJavaSource.replaceQuantArrayCalls("daikon.Quant.fuzzy.eq(x, 0.0)"));
  }
}
