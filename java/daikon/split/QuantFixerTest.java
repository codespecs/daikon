package daikon.split;

import static org.junit.Assert.assertEquals;

import jtb.ParseException;
import org.junit.Test;

/** Tests {@link QuantFixer}. */
public class QuantFixerTest {

  /**
   * Asserts that {@link QuantFixer#fixQuant} converts the input to the expected output.
   *
   * @param expected the expected output
   * @param input the input
   */
  private static void check(String expected, String input) throws ParseException {
    assertEquals(expected, QuantFixer.fixQuant(input));
  }

  /** Tests {@link QuantFixer#fixQuant}. */
  @Test
  public void testFixQuant() throws ParseException {
    check("x==1", "x == 1");
    check("this.a[i]!=null", "daikon.Quant.getElement_Object(this.a, i) != null");
    check("this.a.length-1!=0", "daikon.Quant.size(this.a)-1 != 0");
    check(
        "orig(this.a)[i]==orig(this.a).length",
        "daikon.Quant.getElement_int(orig(this.a), i) == daikon.Quant.size(orig(this.a))");
    // Nested calls are not parenthesized, so that b is recognized as an array of indices.
    check(
        "a[b[f(x,y)]]==a.length",
        "daikon.Quant.getElement_int(a, daikon.Quant.getElement_int(b, f(x, y)))"
            + " == daikon.Quant.size(a)");
    // An array argument that is not a primary expression is parenthesized.
    check(
        "(p?a:b)[i]==(p?a:b).length",
        "daikon.Quant.getElement_int(p ? a : b, i) == daikon.Quant.size(p ? a : b)");
    // String literals may contain parentheses and commas.
    check("a[s.indexOf(\"),\")]==0", "daikon.Quant.getElement_int(a, s.indexOf(\"),\")) == 0");
    // Other names and daikon.Quant methods are left alone.
    check("daikon.Quant.fuzzy.eq(x,0.0)", "daikon.Quant.fuzzy.eq(x, 0.0)");
    check("daikon.Quant.memberOf(x,a)", "daikon.Quant.memberOf(x, a)");
    check("my.daikon.Quant.size(a)==0", "my.daikon.Quant.size(a) == 0");
    // A call with the wrong number of arguments is left alone, but calls within it are converted.
    check(
        "daikon.Quant.getElement_int(a,b.length,j)",
        "daikon.Quant.getElement_int(a, daikon.Quant.size(b), j)");
  }

  /** Tests that {@link PrefixFixer} leaves daikon.Quant names alone. */
  @Test
  public void testPrefixFixer() throws ParseException {
    assertEquals(
        "daikon.Quant.fuzzy.eq(x_y,0.0)", PrefixFixer.fixPrefix("daikon.Quant.fuzzy.eq(x.y, 0.0)"));
  }
}
