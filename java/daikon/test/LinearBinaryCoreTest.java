package daikon.test;

import static org.junit.Assert.assertEquals;

import daikon.inv.binary.twoScalar.LinearBinaryCore;
import org.junit.Test;

/** Tests the formatting methods of {@link LinearBinaryCore}. */
public class LinearBinaryCoreTest {

  /** Tests {@link LinearBinaryCore#parenthesizeIfCompound}. */
  @Test
  public void testParenthesizeIfCompound() {
    assertEquals("x", LinearBinaryCore.parenthesizeIfCompound("x"));
    assertEquals("this.x", LinearBinaryCore.parenthesizeIfCompound("this.x"));
    assertEquals("size(a[])", LinearBinaryCore.parenthesizeIfCompound("size(a[])"));
    assertEquals("a[i-1]", LinearBinaryCore.parenthesizeIfCompound("a[i-1]"));
    assertEquals("orig(x+1)", LinearBinaryCore.parenthesizeIfCompound("orig(x+1)"));
    assertEquals("::global", LinearBinaryCore.parenthesizeIfCompound("::global"));
    assertEquals("p->next", LinearBinaryCore.parenthesizeIfCompound("p->next"));
    assertEquals("s.equals(\"a-b\")", LinearBinaryCore.parenthesizeIfCompound("s.equals(\"a-b\")"));
    assertEquals("(size(a[])-1)", LinearBinaryCore.parenthesizeIfCompound("size(a[])-1"));
    assertEquals("(post(x)+1)", LinearBinaryCore.parenthesizeIfCompound("post(x)+1"));
    assertEquals("(-x)", LinearBinaryCore.parenthesizeIfCompound("-x"));
    assertEquals("(x - 1)", LinearBinaryCore.parenthesizeIfCompound("x - 1"));
  }

  /** Tests {@link LinearBinaryCore#formatTerm}. */
  @Test
  public void testFormatTerm() {
    assertEquals("", LinearBinaryCore.formatTerm(0, "x-1", true));
    assertEquals("x-1", LinearBinaryCore.formatTerm(1, "x-1", true));
    assertEquals(" + x-1", LinearBinaryCore.formatTerm(1, "x-1", false));
    assertEquals("- (x-1)", LinearBinaryCore.formatTerm(-1, "x-1", true));
    assertEquals(" - (x-1)", LinearBinaryCore.formatTerm(-1, "x-1", false));
    assertEquals(" + 2 * (x-1)", LinearBinaryCore.formatTerm(2, "x-1", false));
    assertEquals(" - 2 * (x-1)", LinearBinaryCore.formatTerm(-2, "x-1", false));
    assertEquals(" - 2 * x", LinearBinaryCore.formatTerm(-2, "x", false));
    assertEquals(" - 3", LinearBinaryCore.formatTerm(-3, null, false));
  }
}
