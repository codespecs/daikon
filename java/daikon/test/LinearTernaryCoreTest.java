package daikon.test;

import java.io.*;
import java.util.*;
import junit.framework.*;

import daikon.inv.ternary.threeScalar.*;

public class LinearTernaryCoreTest
  extends TestCase
{

  // for convenience
  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(LinearTernaryCoreTest.class));
  }

  public LinearTernaryCoreTest(String name) {
    super(name);
  }

  void set_cache(LinearTernaryCore ltc, int index, long x, long y, long z) {
    ltc.x_cache[index] = x;
    ltc.y_cache[index] = y;
    ltc.z_cache[index] = z;
  }

  void one_test_set_tri_linear(int[][] triples, long goal_a, long goal_b, long goal_c) {
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    for (int i=0; i<triples.length; i++) {
      assertTrue(triples[i].length == 3);
      ltc.x_cache[i] = triples[i][0];
      ltc.y_cache[i] = triples[i][1];
      ltc.z_cache[i] = triples[i][2];
    }
    ltc.set_tri_linear(new int[] {0,1,2});
    // System.out.println("goals: " + goal_a + " " + goal_b + " " + goal_c);
    // System.out.println("actual: " + ltc.a + " " + ltc.b + " " + ltc.c);
    // System.out.println("difference: " + (goal_a - ltc.a) + " " + (goal_b - ltc.b) + " " + (goal_c - ltc.c));
    assertTrue(ltc.a == goal_a && ltc.b == goal_b && ltc.c == goal_c);
  }

  public void test_set_tri_linear() {
    LinearTernaryCore ltc = new LinearTernaryCore(null);

    one_test_set_tri_linear(new int[][] { { 1, 2, 1 },
                                          { 2, 1, 7 },
                                          { 3, 3, 7 } },
                            4, -2, 1);
    //     # like the above, but swap y and z; results in division-by-zero problem
    //     # tri_linear_relationship((1,1,2),(2,7,1),(3,7,3))
    one_test_set_tri_linear(new int[][] { { 1, 2, 6 },
                                          { 2, 1, -4 },
                                          { 3, 3, 7 } },
                            -3, 7, -5);

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

  public void one_test_format(double a, double b, double c, String result) {
    LinearTernaryCore ltc = new LinearTernaryCore(null);
    ltc.a = a;
    ltc.b = b;
    ltc.c = c;
    // System.out.println("Expecting: " + result);
    // System.out.println("Actual:    " + ltc.format("x", "y", "z"));
    assertTrue(ltc.format("x", "y", "z").equals(result));
  }

  public void test_format() {
    // Need tests with all combinations of: integer/noninteger, and values
    // -1,0,1,other.
    one_test_format(1, 2, 3, "z == x + 2 * y + 3");
    one_test_format(-1, 2, 3, "z == - x + 2 * y + 3");
    one_test_format(-1, -2, 3, "z == - x - 2 * y + 3");
    one_test_format(-1, -2, -3, "z == - x - 2 * y - 3");
    one_test_format(-1, 2, 0, "z == - x + 2 * y");
    one_test_format(-1, 0, 3, "z == - x + 3");
    one_test_format(0, -2, -3, "z ==  - 2 * y - 3");
    one_test_format(-1, 1, 0, "z == - x + y");
    one_test_format(-1, -1, 3, "z == - x - y + 3");
    one_test_format(3, -2, -3, "z == 3 * x - 2 * y - 3");
    one_test_format(3.2, -2.2, -3.4, "z == 3.2 * x - 2.2 * y - 3.4");
    one_test_format(3.0, -2.0, -3.0, "z == 3 * x - 2 * y - 3");
    one_test_format(-1.0, 1.0, 0.0, "z == - x + y");
  }
}
