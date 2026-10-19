package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;

import daikon.FileIO.VarDefinition;
import daikon.ProglangType;
import daikon.VarInfo.VarKind;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.junit.Test;

/** Tests the checking of min-value and max-value records in a decls file. */
public class VarDefinitionBoundTest {

  /**
   * Returns a variable definition with the given rep type and bounds.
   *
   * @param repType the rep type of the variable
   * @param minValue the min-value bound, or null
   * @param maxValue the max-value bound, or null
   * @return a variable definition with the given rep type and bounds
   */
  private static VarDefinition vardef(
      ProglangType repType, @Nullable String minValue, @Nullable String maxValue) {
    VarDefinition result = new VarDefinition("x", VarKind.VARIABLE, repType);
    result.min_value = minValue;
    result.max_value = maxValue;
    return result;
  }

  /** Tests bounds that are valid for their rep type. */
  @Test
  public void testValidBounds() {
    vardef(ProglangType.INT, "-9223372036854775808", "9223372036854775807").checkRep();
    vardef(ProglangType.DOUBLE, "0.25", "1e9").checkRep();
    vardef(ProglangType.STRING, "abc", "xyz").checkRep();
  }

  /** Tests an integral bound that does not fit in a long. */
  @Test
  public void testIntegralBoundOutOfRange() {
    VarDefinition vardef = vardef(ProglangType.INT, null, "18446744073709551615");
    AssertionError e = assertThrows(AssertionError.class, vardef::checkRep);
    assertEquals(
        "max-value '18446744073709551615' for variable x is not a valid int value",
        String.valueOf(e.getMessage()));
  }

  /** Tests an integral bound that is not an integer. */
  @Test
  public void testIntegralBoundNotInteger() {
    VarDefinition vardef = vardef(ProglangType.INT, "100.0", null);
    AssertionError e = assertThrows(AssertionError.class, vardef::checkRep);
    assertEquals(
        "min-value '100.0' for variable x is not a valid int value",
        String.valueOf(e.getMessage()));
  }

  /** Tests a floating-point bound that is not a number. */
  @Test
  public void testFloatBoundNotNumber() {
    VarDefinition vardef = vardef(ProglangType.DOUBLE, "abc", null);
    AssertionError e = assertThrows(AssertionError.class, vardef::checkRep);
    assertEquals(
        "min-value 'abc' for variable x is not a valid double value",
        String.valueOf(e.getMessage()));
  }
}
