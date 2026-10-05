package daikon.test;

import static java.util.logging.Level.INFO;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import daikon.FileIO;
import daikon.inv.OutputFormat;
import daikon.inv.ternary.threeScalar.LinearTernaryCore;
import org.junit.BeforeClass;
import org.junit.Test;

/** Daikon unit test class. */
public class LinearTernaryCoreTest {

  /** prepare for tests */
  @BeforeClass
  public static void setUpClass() {
    daikon.LogHelper.setupLogs(INFO);
    FileIO.new_decl_format = true;
  }

  void set_cache(LinearTernaryCore ltc, int index, long x, long y, long z) {
    ltc.def_points[index] = new LinearTernaryCore.Point(x, y, z);
  }

  void one_test_set_tri_linear(
      int[][] triples, double goal_a, double goal_b, double goal_c, double goal_d) {
    @SuppressWarnings("nullness") // testing code: wrapper will never be used
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    for (int i = 0; i < triples.length; i++) {
      assertEquals(3, triples[i].length);
      set_cache(ltc, i, triples[i][0], triples[i][1], triples[i][2]);
    }
    double[] coef;
    try {
      coef = ltc.calc_tri_linear(ltc.def_points);
    } catch (ArithmeticException e) {
      // In the future, we should perhaps test triples that don't
      // determine a plane; but none of the current ones do.
      throw new Error("Not reached");
      // throw new Error();
      // coef = null; // not reached
    }
    //  System.out.println("goals: " + goal_a + " " + goal_b + " " + goal_c + " " + goal_d);
    //  System.out.println("actual: " + coef[0] + " " + coef[1] + " " + coef[2] + " " + coef[3]);
    // System.out.println("difference: " + (goal_a - ltc.a) + " " + (goal_b - ltc.b) + " " + (goal_c
    // - ltc.c));
    assertEquals(goal_a, coef[0], 0);
    assertEquals(goal_b, coef[1], 0);
    assertEquals(goal_c, coef[2], 0);
    assertEquals(goal_d, coef[3], 0);
  }

  @Test
  public void test_set_tri_linear() {
    one_test_set_tri_linear(new int[][] {{1, 2, 1}, {2, 1, 7}, {3, 3, 7}}, 4, -2, -1, 1);
    one_test_set_tri_linear(
        new int[][] {{1, 2, 6}, {2, 1, -4}, {3, 3, 7}},
        //    -3, 7, -1, -5);
        3,
        -7,
        1,
        5);

    // These have non-integer parameters; must have a LinearTernaryCoreFloat
    // in order to handle them.
    //
    // // a - 3 b + 2 c = -9.5
    // // 0.5 a + 4 b - 10 c = 9
    // // 3 a + 0.1 b + 2 c = -2.2
    // //   solution = -9.5, 9, -2.2
    // // Restated:
    // // .5 a - 1.5 b + c = -4.75
    // // -0.05 a - .4 b + c = -.9
    // // 1.5 a + 0.05 b + c = -1.1
    // //   solution = -9.5, 9, -2.2
    // one_test_set_tri_linear(new float[][] { { .5, -1.5, -4.75 },
    //                                       { -0.05, -.4, -.9 },
    //                                       { 1.5, 0.05, -1.1 } },
    //                         -9.5, 9, -2.2);
    //
    // // Another example:
    // //   2x + 3y + 1/3z = 10
    // //      3x + 4y + 1z = 17
    // //      2y + 7z = 46
    // //
    // //   Solution:
    // //   x = 1
    // //      y = 2
    // //      z = 6
  }

  void one_test_set_tri_linear(
      long[][] triples, double goal_a, double goal_b, double goal_c, double goal_d) {
    @SuppressWarnings("nullness") // testing code: wrapper will never be used
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    for (int i = 0; i < triples.length; i++) {
      assertEquals(3, triples[i].length);
      set_cache(ltc, i, triples[i][0], triples[i][1], triples[i][2]);
    }
    double[] coef = ltc.calc_tri_linear(ltc.def_points);
    assertEquals(goal_a, coef[0], 0);
    assertEquals(goal_b, coef[1], 0);
    assertEquals(goal_c, coef[2], 0);
    assertEquals(goal_d, coef[3], 0);
  }

  /** Tests values whose intermediate products overflow a long. */
  @Test
  public void test_set_tri_linear_large() {
    // x + y - z + 7 == 0
    long p = 3_000_000_000_000_000_000L;
    long q = -2_500_000_000_000_000_000L;
    long r = 1_234_567_890_123_456_789L;
    one_test_set_tri_linear(
        new long[][] {{p, q, p + q + 7}, {q, r, q + r + 7}, {r, -p, r - p + 7}}, 1, 1, -1, 7);
  }

  /** Tests that large values are checked against the plane exactly. */
  @Test
  public void test_fits_plane_large() {
    @SuppressWarnings("nullness") // testing code: wrapper will never be used
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    // x + y - z == 0
    ltc.a = 1;
    ltc.b = 1;
    ltc.c = -1;
    ltc.d = 0;
    // A double cannot represent these values exactly.
    long x = -2_512_781_428_987_574_916L;
    long y = -2_921_747_708_120_997_868L;
    assertTrue(ltc.fits_plane(x, y, x + y));
    assertFalse(ltc.fits_plane(x, y, x + y + 1));
    // A point that is off the plane because z wrapped around.
    assertFalse(ltc.fits_plane(Long.MAX_VALUE, 1, Long.MIN_VALUE));

    // 2x - y - z == 0
    ltc.a = 2;
    ltc.b = -1;
    ltc.c = -1;
    ltc.d = 0;
    // 2x overflows a long, but the point is on the plane.
    long big = 5_000_000_000_000_000_001L;
    assertTrue(ltc.fits_plane(big, big, big));
    assertFalse(ltc.fits_plane(big, big, big - 1));
  }

  public void one_test_format(double a, double b, double c, double d, String goal_result) {
    @SuppressWarnings("nullness") // testing code: wrapper will never be used
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    ltc.a = a;
    ltc.b = b;
    ltc.c = c;
    ltc.d = d;
    String actual_result = ltc.format_using(OutputFormat.DAIKON, "x", "y", "z");
    //    System.out.println("Expecting: " + goal_result);
    //    System.out.println("Actual:    " + actual_result);
    assertEquals(goal_result, actual_result);
  }

  @Test
  public void test_format() {
    // Need tests with all combinations of: integer/noninteger, and values
    // -1,0,1,other.
    one_test_format(1, 2, 1, 3, "x + 2 * y + z + 3 == 0");
    one_test_format(-1, 2, 1, 3, "- x + 2 * y + z + 3 == 0");
    one_test_format(-1, -2, -1, 3, "- x - 2 * y - z + 3 == 0");
    one_test_format(-1, -2, -4, -3, "- x - 2 * y - 4 * z - 3 == 0");
    one_test_format(-1, 2, 3, 0, "- x + 2 * y + 3 * z == 0");
    one_test_format(-1, 0, 0, 3, "- x + 3 == 0");
    one_test_format(0, -2, 5, -3, "- 2 * y + 5 * z - 3 == 0");
    one_test_format(-1, 1, -2, 0, "- x + y - 2 * z == 0");
    one_test_format(-1, -1, 2, 3, "- x - y + 2 * z + 3 == 0");
    one_test_format(3, -2, 0, -3, "3 * x - 2 * y - 3 == 0");
    // hmmm, we can't actually have this test because there are never any double coeffs, they're not
    // calculated as such and are converted to ints
    //  one_test_format(3.2, -2.2, 1.4, -3.4, "3.2 * x - 2.2 * y + 1.4 * z - 3.4 == 0");
    one_test_format(3.0, -2.0, 2.0, -3.0, "3 * x - 2 * y + 2 * z - 3 == 0");
    one_test_format(-1.0, 1.0, 0.0, 0.0, "- x + y == 0");
  }
}
