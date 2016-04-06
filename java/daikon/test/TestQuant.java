package daikon.test;

import daikon.*;
import junit.framework.*;
import java.util.*;
import java.io.*;
import plume.*;

@SuppressWarnings("nullness") // testing code
public final class TestQuant extends TestCase {

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(TestPlume.class));
  }

  public TestQuant(String name) {
    super(name);
  }

  public static final void assert_arrays_equals(int[] a1, int[] a2) {
    TestPlume.assert_arrays_equals(a1, a2);
  }

  private static int[] removeAdjacentDups(int[] a) {
    if (a.length == 0) {
      return new int[] {};
    }
    int[] intermediate = new int[a.length];
    int length = 0;
    for (int i = 0; i < a.length - 1; i++) {
      if (a[i] != a[i + 1]) {
        intermediate[length++] = a[i];
      }
    }
    intermediate[length++] = a[a.length - 1];
    int[] retval = new int[length];
    for (int i = 0; i < length; i++) {
      retval[i] = intermediate[i];
    }
    return retval;
  }

  // These methods aren't used to express any invariants; no need for them.
  //   public static void test_min() {
  //     assert Quant.min(new int[] { 1,2,3 }) == 1;
  //     assert Quant.min(new int[] { 2,33,1 }) == 1;
  //     assert Quant.min(new int[] { 3,-2,1 }) == -2;
  //     assert Quant.min(new int[] { 3 }) == 3;
  //   }

  //   public static void test_max() {
  //     assert Quant.max(new int[] { 1,2,3 }) == 3;
  //     assert Quant.max(new int[] { 2,33,1 }) == 33;
  //     assert Quant.max(new int[] { 3,-2,1 }) == 3;
  //     assert Quant.max(new int[] { 3 }) == 3;
  //   }

  public static void test_concat() {
    assert_arrays_equals(Quant.concat(new int[] {}, new int[] {}), new int[] {});
    assert_arrays_equals(Quant.concat(new int[] {1}, new int[] {}), new int[] {1});
    assert_arrays_equals(Quant.concat(new int[] {}, new int[] {1}), new int[] {1});
    assert_arrays_equals(Quant.concat(new int[] {1}, new int[] {1}), new int[] {1, 1});
    assert_arrays_equals(
        Quant.concat(new int[] {1, 2, 3}, new int[] {3, 4, 5}), new int[] {1, 2, 3, 3, 4, 5});
    assert_arrays_equals(
        Quant.concat(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5}),
        new int[] {-1, 2, 3, 4, 5});
    assert_arrays_equals(
        Quant.concat(new int[] {-1, 2, 3, 4}, new int[] {5}), new int[] {-1, 2, 3, 4, 5});
  }

  public static void test_union() {
    {
      int[] u = Quant.union(new int[] {}, new int[] {});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
    {
      int[] u = Quant.union(new int[] {1}, new int[] {});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1});
    }
    {
      int[] u = Quant.union(new int[] {}, new int[] {1});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1});
    }
    {
      int[] u = Quant.union(new int[] {1}, new int[] {1});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1});
    }
    {
      int[] u = Quant.union(new int[] {1, 2, 3}, new int[] {3, 4, 5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1, 2, 3, 4, 5});
    }
    {
      int[] u = Quant.union(new int[] {1, 2, 3}, new int[] {1, 2, 3});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1, 2, 3});
    }
    {
      int[] u = Quant.union(new int[] {-1}, new int[] {2, 3, 4, 5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {-1, 2, 3, 4, 5});
    }
    {
      int[] u = Quant.union(new int[] {-1, 2, 3, 4}, new int[] {5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {-1, 2, 3, 4, 5});
    }
  }

  public static void test_intersection() {
    {
      int[] u = Quant.intersection(new int[] {}, new int[] {});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
    {
      int[] u = Quant.intersection(new int[] {1}, new int[] {});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
    {
      int[] u = Quant.intersection(new int[] {}, new int[] {1});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
    {
      int[] u = Quant.intersection(new int[] {1}, new int[] {1});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1});
    }
    {
      int[] u = Quant.intersection(new int[] {1, 2, 3}, new int[] {1, 2, 3});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {1, 2, 3});
    }
    {
      int[] u = Quant.intersection(new int[] {1, 2, 3}, new int[] {3, 4, 5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {3});
    }
    {
      int[] u = Quant.intersection(new int[] {-1}, new int[] {2, 3, 4, 5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
    {
      int[] u = Quant.intersection(new int[] {-1, 2, 3, 4}, new int[] {5});
      Arrays.sort(u);
      assert_arrays_equals(removeAdjacentDups(u), new int[] {});
    }
  }

  public static void test_setDiff() {
    assert_arrays_equals(Quant.setDiff(new int[] {}, new int[] {}), new int[] {});
    assert_arrays_equals(Quant.setDiff(new int[] {1}, new int[] {}), new int[] {1});
    assert_arrays_equals(Quant.setDiff(new int[] {}, new int[] {1}), new int[] {});
    assert_arrays_equals(Quant.setDiff(new int[] {1}, new int[] {1}), new int[] {});
    assert_arrays_equals(Quant.setDiff(new int[] {1, 2, 3}, new int[] {1, 2, 3}), new int[] {});
    assert_arrays_equals(Quant.setDiff(new int[] {1, 2, 3}, new int[] {3, 4, 5}), new int[] {1, 2});
    assert_arrays_equals(
        Quant.setDiff(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5}),
        new int[] {-1});
    assert_arrays_equals(
        Quant.setDiff(new int[] {-1, 2, 3, 4}, new int[] {5}), new int[] {-1, 2, 3, 4});
  }

  public static void test_setEqual() {
    assert Quant.setEqual(new int[] {}, new int[] {}) == true;
    assert Quant.setEqual(new int[] {1}, new int[] {}) == false;
    assert Quant.setEqual(new int[] {}, new int[] {1}) == false;
    assert Quant.setEqual(new int[] {1}, new int[] {1}) == true;
    assert Quant.setEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.setEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.setEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
    assert Quant.setEqual(new int[] {1, 2, 3}, new int[] {3, 4, 5}) == false;
    assert Quant.setEqual(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5})
        == false;
    assert Quant.setEqual(new int[] {-1, 2, 3, 4}, new int[] {5}) == false;
  }

  public static void test_subsetOf() {
    assert Quant.subsetOf(new int[] {-1}, new int[] {-1, 1}) == true;
    assert Quant.subsetOf(new int[] {-1, 0, 1}, new int[] {-1, 1}) == false;
    assert Quant.subsetOf(new int[] {-1, 1}, new int[] {-1, 1}) == true;
    assert Quant.subsetOf(new int[] {-1, 2, 3, 4}, new int[] {5}) == false;
    assert Quant.subsetOf(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5})
        == false;
    assert Quant.subsetOf(new int[] {1}, new int[] {-1, 1}) == true;
    assert Quant.subsetOf(new int[] {1}, new int[] {1}) == true;
    assert Quant.subsetOf(new int[] {1}, new int[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new int[] {1}, new int[] {}) == false;
    assert Quant.subsetOf(new int[] {1, 2}, new int[] {1}) == false;
    assert Quant.subsetOf(new int[] {1, 2}, new int[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new int[] {1, 2}, new int[] {}) == false;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2}) == false;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2, 3}) == false;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
    assert Quant.subsetOf(new int[] {1, 2, 3}, new int[] {3, 4, 5}) == false;
    assert Quant.subsetOf(new int[] {2, 1}, new int[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new int[] {2, 3}, new int[] {3, 3, 3, 3}) == false;
    assert Quant.subsetOf(new int[] {3, 1}, new int[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new int[] {3, 3}, new int[] {3, 3, 3, 3}) == true;
    assert Quant.subsetOf(new int[] {}, new int[] {-1, 1}) == true;
    assert Quant.subsetOf(new int[] {}, new int[] {1}) == true;
    assert Quant.subsetOf(new int[] {}, new int[] {2, 3, 1}) == true;
    assert Quant.subsetOf(new int[] {}, new int[] {}) == true;
  }

  public static void test_subsetOf_different_types() {
    assert Quant.subsetOf(new byte[] {}, new int[] {}) == true;
    assert Quant.subsetOf(new long[] {}, new long[] {}) == true;
    assert Quant.subsetOf(new byte[] {1}, new long[] {}) == false;
    assert Quant.subsetOf(new short[] {1, 2}, new short[] {}) == false;
    assert Quant.subsetOf(new long[] {}, new short[] {1}) == true;
    assert Quant.subsetOf(new int[] {1}, new short[] {1}) == true;
    assert Quant.subsetOf(new float[] {1, 2}, new double[] {1}) == false;
    assert Quant.subsetOf(new double[] {}, new double[] {-1, 1}) == true;
    assert Quant.subsetOf(new float[] {1}, new float[] {-1, 1}) == true;
    assert Quant.subsetOf(new double[] {-1}, new float[] {-1, 1}) == true;
    assert Quant.subsetOf(new byte[] {-1, 1}, new short[] {-1, 1}) == true;
    assert Quant.subsetOf(new short[] {-1, 0, 1}, new byte[] {-1, 1}) == false;
    assert Quant.subsetOf(new int[] {1, 2}, new byte[] {1, 2, 3}) == true;
    assert Quant.subsetOf(new long[] {}, new short[] {2, 3, 1}) == true;
  }

  public static void test_isReverse() {
    assert Quant.isReverse(new int[] {}, new int[] {}) == true;
    assert Quant.isReverse(new int[] {1}, new int[] {}) == false;
    assert Quant.isReverse(new int[] {}, new int[] {1}) == false;
    assert Quant.isReverse(new int[] {1}, new int[] {1}) == true;
    assert Quant.isReverse(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.isReverse(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.isReverse(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
    assert Quant.isReverse(new int[] {1, 2, 3}, new int[] {3, 4, 5}) == false;
    assert Quant.isReverse(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5})
        == false;
    assert Quant.isReverse(new int[] {-1, 2, 3, 4}, new int[] {5}) == false;
  }

  public static void test_noDups() {
    assert Quant.noDups(new int[] {}) == true;
    assert Quant.noDups(new int[] {1}) == true;
    assert Quant.noDups(new int[] {-1}) == true;
    assert Quant.noDups(new int[] {-1, 1}) == true;
    assert Quant.noDups(new int[] {1, 2, 3}) == true;
    assert Quant.noDups(new int[] {2, 3, 1}) == true;
    assert Quant.noDups(new int[] {2, 3, 2, 3}) == false;
    assert Quant.noDups(new int[] {2, 3, 3, 2}) == false;
    assert Quant.noDups(new int[] {3, 3, 3, 3}) == false;
    assert Quant.noDups(new int[] {3, 3, 3, 2}) == false;
    assert Quant.noDups(new int[] {1, 1, 1, -1}) == false;
    assert Quant.noDups(new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
  }

  public static void test_memberOf() {
    assert Quant.memberOf(1, new int[] {}) == false;
    assert Quant.memberOf(1, new int[] {1}) == true;
    assert Quant.memberOf(-1, new int[] {1}) == false;
    assert Quant.memberOf(1, new int[] {-1}) == false;
    assert Quant.memberOf(-1, new int[] {-1}) == true;
    assert Quant.memberOf(1, new int[] {-1, 1}) == true;
    assert Quant.memberOf(-1, new int[] {-1, 1}) == true;
    assert Quant.memberOf(2, new int[] {-1, 1}) == false;
    assert Quant.memberOf(1, new int[] {1, 2, 3}) == true;
    assert Quant.memberOf(2, new int[] {1, 2, 3}) == true;
    assert Quant.memberOf(3, new int[] {1, 2, 3}) == true;
    assert Quant.memberOf(4, new int[] {1, 2, 3}) == false;
    assert Quant.memberOf(1, new int[] {2, 3, 1}) == true;
    assert Quant.memberOf(2, new int[] {2, 3, 1}) == true;
    assert Quant.memberOf(3, new int[] {2, 3, 1}) == true;
    assert Quant.memberOf(4, new int[] {2, 3, 1}) == false;
    assert Quant.memberOf(3, new int[] {3, 3, 3, 3}) == true;
    assert Quant.memberOf(2, new int[] {3, 3, 3, 3}) == false;
    assert Quant.memberOf(-1, new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
    assert Quant.memberOf(1, new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
    assert Quant.memberOf(6, new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
  }

  public static void test_slice() {
    assert_arrays_equals(Quant.slice(new int[] {}, 0, 0), new int[] {});
    assert_arrays_equals(Quant.slice(new int[] {1}, 0, 0), new int[] {1});
    assert_arrays_equals(Quant.slice(new int[] {1}, 0, 1), new int[] {});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 0, 0), new int[] {1});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 0, 1), new int[] {1, 2});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 0, 2), new int[] {1, 2, 3});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 0, 3), new int[] {});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 1, 1), new int[] {2});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 1, 2), new int[] {2, 3});
    assert_arrays_equals(Quant.slice(new int[] {1, 2, 3}, 2, 2), new int[] {3});
  }

  public static void test_eltsEqual() {
    assert Quant.eltsEqual(new int[] {}, 0) == true;
    assert Quant.eltsEqual(new int[] {1}, -1) == false;
    assert Quant.eltsEqual(new int[] {1}, 1) == true;
    assert Quant.eltsEqual(new int[] {-1}, 1) == false;
    assert Quant.eltsEqual(new int[] {-1}, -1) == true;
    assert Quant.eltsEqual(new int[] {-1, 1}, 1) == false;
    assert Quant.eltsEqual(new int[] {-1, 1}, -1) == false;
    assert Quant.eltsEqual(new int[] {1, 2, 3}, 1) == false;
    assert Quant.eltsEqual(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsEqual(new int[] {1, 2, 3}, 3) == false;
    assert Quant.eltsEqual(new int[] {2, 3, 2, 3}, 2) == false;
    assert Quant.eltsEqual(new int[] {2, 3, 2, 3}, 3) == false;
    assert Quant.eltsEqual(new int[] {3, 3, 3, 3}, 3) == true;
    assert Quant.eltsEqual(new int[] {1, 1, 1, -1}, 1) == false;
    assert Quant.eltsEqual(new int[] {1, 1, 1, 1}, 1) == true;
  }

  public static void test_eltsNotEqual() {
    assert Quant.eltsNotEqual(new int[] {}, 0) == true;
    assert Quant.eltsNotEqual(new int[] {1}, -1) == true;
    assert Quant.eltsNotEqual(new int[] {1}, 1) == false;
    assert Quant.eltsNotEqual(new int[] {-1}, 1) == true;
    assert Quant.eltsNotEqual(new int[] {-1}, -1) == false;
    assert Quant.eltsNotEqual(new int[] {-1, 1}, 1) == false;
    assert Quant.eltsNotEqual(new int[] {-1, 1}, -1) == false;
    assert Quant.eltsNotEqual(new int[] {1, 2, 3}, 1) == false;
    assert Quant.eltsNotEqual(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsNotEqual(new int[] {1, 2, 3}, 3) == false;
    assert Quant.eltsNotEqual(new int[] {2, 3, 2, 3}, 2) == false;
    assert Quant.eltsNotEqual(new int[] {2, 3, 2, 3}, 3) == false;
    assert Quant.eltsNotEqual(new int[] {3, 3, 3, 3}, 3) == false;
    assert Quant.eltsNotEqual(new int[] {1, 1, 1, -1}, 1) == false;
    assert Quant.eltsNotEqual(new int[] {-1, -1, -1, -1}, 1) == true;
    assert Quant.eltsNotEqual(new int[] {1, 1, 1, 1}, 1) == false;
  }

  public static void test_eltsGT() {
    assert Quant.eltsGT(new int[] {}, 0) == true;
    assert Quant.eltsGT(new int[] {1}, -1) == true;
    assert Quant.eltsGT(new int[] {1}, 1) == false;
    assert Quant.eltsGT(new int[] {-1}, 1) == false;
    assert Quant.eltsGT(new int[] {-1}, -1) == false;
    assert Quant.eltsGT(new int[] {-1, 1}, 1) == false;
    assert Quant.eltsGT(new int[] {-1, 1}, -1) == false;
    assert Quant.eltsGT(new int[] {1, 2, 3}, 0) == true;
    assert Quant.eltsGT(new int[] {1, 2, 3}, 1) == false;
    assert Quant.eltsGT(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsGT(new int[] {1, 2, 3}, 3) == false;
    assert Quant.eltsGT(new int[] {1, 2, 3}, 4) == false;
    assert Quant.eltsGT(new int[] {2, 3, 2, 3}, 1) == true;
    assert Quant.eltsGT(new int[] {2, 3, 2, 3}, 2) == false;
    assert Quant.eltsGT(new int[] {2, 3, 2, 3}, 3) == false;
    assert Quant.eltsGT(new int[] {3, 3, 3, 3}, 2) == true;
    assert Quant.eltsGT(new int[] {3, 3, 3, 3}, 3) == false;
  }

  public static void test_eltsGTE() {
    assert Quant.eltsGTE(new int[] {}, 0) == true;
    assert Quant.eltsGTE(new int[] {1}, -1) == true;
    assert Quant.eltsGTE(new int[] {1}, 1) == true;
    assert Quant.eltsGTE(new int[] {-1}, 1) == false;
    assert Quant.eltsGTE(new int[] {-1}, -1) == true;
    assert Quant.eltsGTE(new int[] {-1, 1}, 1) == false;
    assert Quant.eltsGTE(new int[] {-1, 1}, -1) == true;
    assert Quant.eltsGTE(new int[] {1, 2, 3}, 0) == true;
    assert Quant.eltsGTE(new int[] {1, 2, 3}, 1) == true;
    assert Quant.eltsGTE(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsGTE(new int[] {1, 2, 3}, 3) == false;
    assert Quant.eltsGTE(new int[] {1, 2, 3}, 4) == false;
    assert Quant.eltsGTE(new int[] {2, 3, 2, 3}, 1) == true;
    assert Quant.eltsGTE(new int[] {2, 3, 2, 3}, 2) == true;
    assert Quant.eltsGTE(new int[] {2, 3, 2, 3}, 3) == false;
    assert Quant.eltsGTE(new int[] {3, 3, 3, 3}, 2) == true;
    assert Quant.eltsGTE(new int[] {3, 3, 3, 3}, 3) == true;
  }

  public static void test_eltsLT() {
    assert Quant.eltsLT(new int[] {}, 0) == true;
    assert Quant.eltsLT(new int[] {1}, -1) == false;
    assert Quant.eltsLT(new int[] {1}, 1) == false;
    assert Quant.eltsLT(new int[] {-1}, 1) == true;
    assert Quant.eltsLT(new int[] {-1}, -1) == false;
    assert Quant.eltsLT(new int[] {-1, 1}, 1) == false;
    assert Quant.eltsLT(new int[] {-1, 1}, -1) == false;
    assert Quant.eltsLT(new int[] {1, 2, 3}, 0) == false;
    assert Quant.eltsLT(new int[] {1, 2, 3}, 1) == false;
    assert Quant.eltsLT(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsLT(new int[] {1, 2, 3}, 3) == false;
    assert Quant.eltsLT(new int[] {1, 2, 3}, 4) == true;
    assert Quant.eltsLT(new int[] {2, 3, 2, 3}, 1) == false;
    assert Quant.eltsLT(new int[] {2, 3, 2, 3}, 2) == false;
    assert Quant.eltsLT(new int[] {2, 3, 2, 3}, 3) == false;
    assert Quant.eltsLT(new int[] {3, 3, 3, 3}, 2) == false;
    assert Quant.eltsLT(new int[] {3, 3, 3, 3}, 3) == false;
  }

  public static void test_eltsLTE() {
    assert Quant.eltsLTE(new int[] {}, 0) == true;
    assert Quant.eltsLTE(new int[] {1}, -1) == false;
    assert Quant.eltsLTE(new int[] {1}, 1) == true;
    assert Quant.eltsLTE(new int[] {-1}, 1) == true;
    assert Quant.eltsLTE(new int[] {-1}, -1) == true;
    assert Quant.eltsLTE(new int[] {-1, 1}, 1) == true;
    assert Quant.eltsLTE(new int[] {-1, 1}, -1) == false;
    assert Quant.eltsLTE(new int[] {1, 2, 3}, 0) == false;
    assert Quant.eltsLTE(new int[] {1, 2, 3}, 1) == false;
    assert Quant.eltsLTE(new int[] {1, 2, 3}, 2) == false;
    assert Quant.eltsLTE(new int[] {1, 2, 3}, 3) == true;
    assert Quant.eltsLTE(new int[] {1, 2, 3}, 4) == true;
    assert Quant.eltsLTE(new int[] {2, 3, 2, 3}, 1) == false;
    assert Quant.eltsLTE(new int[] {2, 3, 2, 3}, 2) == false;
    assert Quant.eltsLTE(new int[] {2, 3, 2, 3}, 3) == true;
    assert Quant.eltsLTE(new int[] {3, 3, 3, 3}, 2) == false;
    assert Quant.eltsLTE(new int[] {3, 3, 3, 3}, 3) == true;
  }

  public static void test_pairwiseEqual() {
    assert Quant.pairwiseEqual(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseEqual(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseEqual(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseEqual(new int[] {1}, new int[] {1}) == true;
    assert Quant.pairwiseEqual(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseNotEqual() {
    assert Quant.pairwiseNotEqual(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseNotEqual(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseNotEqual(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseNotEqual(new int[] {1}, new int[] {1}) == false;
    assert Quant.pairwiseNotEqual(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseLT() {
    assert Quant.pairwiseLT(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseLT(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseLT(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseLT(new int[] {1}, new int[] {1}) == false;
    assert Quant.pairwiseLT(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == true;
    assert Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseLTE() {
    assert Quant.pairwiseLTE(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseLTE(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseLTE(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseLTE(new int[] {1}, new int[] {1}) == true;
    assert Quant.pairwiseLTE(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == true;
    assert Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseGT() {
    assert Quant.pairwiseGT(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseGT(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseGT(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseGT(new int[] {1}, new int[] {1}) == false;
    assert Quant.pairwiseGT(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == false;
    assert Quant.pairwiseGT(new int[] {2, 3, 4}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseGTE() {
    assert Quant.pairwiseGTE(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseGTE(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseGTE(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseGTE(new int[] {1}, new int[] {1}) == true;
    assert Quant.pairwiseGTE(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == false;
    assert Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == true;
    assert Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_eltwiseEqual() {
    assert Quant.eltwiseEqual(new int[] {}) == true;
    assert Quant.eltwiseEqual(new int[] {1}) == true;
    assert Quant.eltwiseEqual(new int[] {-1}) == true;
    assert Quant.eltwiseEqual(new int[] {-1, 1}) == false;
    assert Quant.eltwiseEqual(new int[] {1, 1}) == true;
    assert Quant.eltwiseEqual(new int[] {1, 2, 3}) == false;
    assert Quant.eltwiseEqual(new int[] {2, 3, 1}) == false;
    assert Quant.eltwiseEqual(new int[] {3, 2, 1}) == false;
    assert Quant.eltwiseEqual(new int[] {2, 3, 3, 3}) == false;
    assert Quant.eltwiseEqual(new int[] {2, 3, 3, 4}) == false;
    assert Quant.eltwiseEqual(new int[] {3, 3, 3, 2}) == false;
    assert Quant.eltwiseEqual(new int[] {4, 3, 3, 2}) == false;
    assert Quant.eltwiseEqual(new int[] {3, 3, 3, 3}) == true;
    assert Quant.eltwiseEqual(new int[] {2, 3, 2, 3}) == false;
    assert Quant.eltwiseEqual(new int[] {-1, 1, 2, 3, 4, 5, 6}) == false;
  }

  public static void test_eltwiseNotEqual() {
    assert Quant.eltwiseNotEqual(new int[] {}) == true;
    assert Quant.eltwiseNotEqual(new int[] {1}) == true;
    assert Quant.eltwiseNotEqual(new int[] {-1}) == true;
    assert Quant.eltwiseNotEqual(new int[] {-1, 1}) == true;
    assert Quant.eltwiseNotEqual(new int[] {1, 1}) == false;
    assert Quant.eltwiseNotEqual(new int[] {1, 2, 3}) == true;
    assert Quant.eltwiseNotEqual(new int[] {2, 3, 1}) == true;
    assert Quant.eltwiseNotEqual(new int[] {3, 2, 1}) == true;
    assert Quant.eltwiseNotEqual(new int[] {2, 3, 3, 3}) == false;
    assert Quant.eltwiseNotEqual(new int[] {2, 3, 3, 4}) == false;
    assert Quant.eltwiseNotEqual(new int[] {3, 3, 3, 2}) == false;
    assert Quant.eltwiseNotEqual(new int[] {4, 3, 3, 2}) == false;
    assert Quant.eltwiseNotEqual(new int[] {3, 3, 3, 3}) == false;
    assert Quant.eltwiseNotEqual(new int[] {2, 3, 2, 3}) == true;
    assert Quant.eltwiseNotEqual(new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
  }

  public static void test_eltwiseLT() {
    assert Quant.eltwiseLT(new int[] {}) == true;
    assert Quant.eltwiseLT(new int[] {1}) == true;
    assert Quant.eltwiseLT(new int[] {-1}) == true;
    assert Quant.eltwiseLT(new int[] {-1, 1}) == true;
    assert Quant.eltwiseLT(new int[] {1, 1}) == false;
    assert Quant.eltwiseLT(new int[] {1, 2, 3}) == true;
    assert Quant.eltwiseLT(new int[] {2, 3, 1}) == false;
    assert Quant.eltwiseLT(new int[] {3, 2, 1}) == false;
    assert Quant.eltwiseLT(new int[] {2, 3, 3, 3}) == false;
    assert Quant.eltwiseLT(new int[] {2, 3, 3, 4}) == false;
    assert Quant.eltwiseLT(new int[] {3, 3, 3, 2}) == false;
    assert Quant.eltwiseLT(new int[] {4, 3, 3, 2}) == false;
    assert Quant.eltwiseLT(new int[] {3, 3, 3, 3}) == false;
    assert Quant.eltwiseLT(new int[] {2, 3, 2, 3}) == false;
    assert Quant.eltwiseLT(new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
  }

  public static void test_eltwiseLTE() {
    assert Quant.eltwiseLTE(new int[] {}) == true;
    assert Quant.eltwiseLTE(new int[] {1}) == true;
    assert Quant.eltwiseLTE(new int[] {-1}) == true;
    assert Quant.eltwiseLTE(new int[] {-1, 1}) == true;
    assert Quant.eltwiseLTE(new int[] {1, 1}) == true;
    assert Quant.eltwiseLTE(new int[] {1, 2, 3}) == true;
    assert Quant.eltwiseLTE(new int[] {2, 3, 1}) == false;
    assert Quant.eltwiseLTE(new int[] {3, 2, 1}) == false;
    assert Quant.eltwiseLTE(new int[] {2, 3, 3, 3}) == true;
    assert Quant.eltwiseLTE(new int[] {2, 3, 3, 4}) == true;
    assert Quant.eltwiseLTE(new int[] {3, 3, 3, 2}) == false;
    assert Quant.eltwiseLTE(new int[] {4, 3, 3, 2}) == false;
    assert Quant.eltwiseLTE(new int[] {3, 3, 3, 3}) == true;
    assert Quant.eltwiseLTE(new int[] {2, 3, 2, 3}) == false;
    assert Quant.eltwiseLTE(new int[] {-1, 1, 2, 3, 4, 5, 6}) == true;
  }

  public static void test_eltwiseGT() {
    assert Quant.eltwiseGT(new int[] {}) == true;
    assert Quant.eltwiseGT(new int[] {1}) == true;
    assert Quant.eltwiseGT(new int[] {-1}) == true;
    assert Quant.eltwiseGT(new int[] {-1, 1}) == false;
    assert Quant.eltwiseGT(new int[] {1, 1}) == false;
    assert Quant.eltwiseGT(new int[] {1, 2, 3}) == false;
    assert Quant.eltwiseGT(new int[] {2, 3, 1}) == false;
    assert Quant.eltwiseGT(new int[] {3, 2, 1}) == true;
    assert Quant.eltwiseGT(new int[] {2, 3, 3, 3}) == false;
    assert Quant.eltwiseGT(new int[] {2, 3, 3, 4}) == false;
    assert Quant.eltwiseGT(new int[] {3, 3, 3, 2}) == false;
    assert Quant.eltwiseGT(new int[] {4, 3, 3, 2}) == false;
    assert Quant.eltwiseGT(new int[] {3, 3, 3, 3}) == false;
    assert Quant.eltwiseGT(new int[] {2, 3, 2, 3}) == false;
    assert Quant.eltwiseGT(new int[] {-1, 1, 2, 3, 4, 5, 6}) == false;
  }

  public static void test_eltwiseGTE() {
    assert Quant.eltwiseGTE(new int[] {}) == true;
    assert Quant.eltwiseGTE(new int[] {1}) == true;
    assert Quant.eltwiseGTE(new int[] {-1}) == true;
    assert Quant.eltwiseGTE(new int[] {-1, 1}) == false;
    assert Quant.eltwiseGTE(new int[] {1, 1}) == true;
    assert Quant.eltwiseGTE(new int[] {1, 2, 3}) == false;
    assert Quant.eltwiseGTE(new int[] {2, 3, 1}) == false;
    assert Quant.eltwiseGTE(new int[] {3, 2, 1}) == true;
    assert Quant.eltwiseGTE(new int[] {2, 3, 3, 3}) == false;
    assert Quant.eltwiseGTE(new int[] {2, 3, 3, 4}) == false;
    assert Quant.eltwiseGTE(new int[] {3, 3, 3, 2}) == true;
    assert Quant.eltwiseGTE(new int[] {4, 3, 3, 2}) == true;
    assert Quant.eltwiseGTE(new int[] {3, 3, 3, 3}) == true;
    assert Quant.eltwiseGTE(new int[] {2, 3, 2, 3}) == false;
    assert Quant.eltwiseGTE(new int[] {-1, 1, 2, 3, 4, 5, 6}) == false;
  }

  public static void test_eltsEqualIndex() {
    assert Quant.eltsEqualIndex(new int[] {}) == true;
    assert Quant.eltsEqualIndex(new int[] {0}) == true;
    assert Quant.eltsEqualIndex(new int[] {1}) == false;
    assert Quant.eltsEqualIndex(new int[] {-1}) == false;
    assert Quant.eltsEqualIndex(new int[] {-1, -1, 1}) == false;
    assert Quant.eltsEqualIndex(new int[] {-1, 0, 1}) == false;
    assert Quant.eltsEqualIndex(new int[] {-1, 1, 1}) == false;
    assert Quant.eltsEqualIndex(new int[] {0, 0, 2}) == false;
    assert Quant.eltsEqualIndex(new int[] {0, 1, 2}) == true;
    assert Quant.eltsEqualIndex(new int[] {0, 2, 2}) == false;
    assert Quant.eltsEqualIndex(new int[] {1, 1, 3}) == false;
    assert Quant.eltsEqualIndex(new int[] {1, 2, 3}) == false;
    assert Quant.eltsEqualIndex(new int[] {1, 3, 3}) == false;
  }

  public static void test_eltsNotEqualIndex() {
    assert Quant.eltsNotEqualIndex(new int[] {}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {0}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {1}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {-1}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {-1, -1, 1}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {-1, 0, 1}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {-1, 1, 1}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {0, 0, 2}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {0, 1, 2}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {0, 2, 2}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {1, 1, 3}) == false;
    assert Quant.eltsNotEqualIndex(new int[] {1, 2, 3}) == true;
    assert Quant.eltsNotEqualIndex(new int[] {1, 3, 3}) == true;
  }

  public static void test_eltsGteIndex() {
    assert Quant.eltsGteIndex(new int[] {}) == true;
    assert Quant.eltsGteIndex(new int[] {0}) == true;
    assert Quant.eltsGteIndex(new int[] {1}) == true;
    assert Quant.eltsGteIndex(new int[] {-1}) == false;
    assert Quant.eltsGteIndex(new int[] {-1, -1, 1}) == false;
    assert Quant.eltsGteIndex(new int[] {-1, 0, 1}) == false;
    assert Quant.eltsGteIndex(new int[] {-1, 1, 1}) == false;
    assert Quant.eltsGteIndex(new int[] {0, 0, 2}) == false;
    assert Quant.eltsGteIndex(new int[] {0, 1, 2}) == true;
    assert Quant.eltsGteIndex(new int[] {0, 2, 2}) == true;
    assert Quant.eltsGteIndex(new int[] {1, 1, 3}) == true;
    assert Quant.eltsGteIndex(new int[] {1, 2, 3}) == true;
    assert Quant.eltsGteIndex(new int[] {1, 3, 3}) == true;
  }

  public static void test_eltsGtIndex() {
    assert Quant.eltsGtIndex(new int[] {}) == true;
    assert Quant.eltsGtIndex(new int[] {0}) == false;
    assert Quant.eltsGtIndex(new int[] {1}) == true;
    assert Quant.eltsGtIndex(new int[] {-1}) == false;
    assert Quant.eltsGtIndex(new int[] {-1, -1, 1}) == false;
    assert Quant.eltsGtIndex(new int[] {-1, 0, 1}) == false;
    assert Quant.eltsGtIndex(new int[] {-1, 1, 1}) == false;
    assert Quant.eltsGtIndex(new int[] {0, 0, 2}) == false;
    assert Quant.eltsGtIndex(new int[] {0, 1, 2}) == false;
    assert Quant.eltsGtIndex(new int[] {0, 2, 2}) == false;
    assert Quant.eltsGtIndex(new int[] {1, 1, 3}) == false;
    assert Quant.eltsGtIndex(new int[] {1, 2, 3}) == true;
    assert Quant.eltsGtIndex(new int[] {1, 3, 3}) == true;
  }

  public static void test_eltsLteIndex() {
    assert Quant.eltsLteIndex(new int[] {}) == true;
    assert Quant.eltsLteIndex(new int[] {0}) == true;
    assert Quant.eltsLteIndex(new int[] {1}) == false;
    assert Quant.eltsLteIndex(new int[] {-1}) == true;
    assert Quant.eltsLteIndex(new int[] {-1, -1, 1}) == true;
    assert Quant.eltsLteIndex(new int[] {-1, 0, 1}) == true;
    assert Quant.eltsLteIndex(new int[] {-1, 1, 1}) == true;
    assert Quant.eltsLteIndex(new int[] {0, 0, 2}) == true;
    assert Quant.eltsLteIndex(new int[] {0, 1, 2}) == true;
    assert Quant.eltsLteIndex(new int[] {0, 2, 2}) == false;
    assert Quant.eltsLteIndex(new int[] {1, 1, 3}) == false;
    assert Quant.eltsLteIndex(new int[] {1, 2, 3}) == false;
    assert Quant.eltsLteIndex(new int[] {1, 3, 3}) == false;
  }

  public static void test_eltsLtIndex() {
    assert Quant.eltsLtIndex(new int[] {}) == true;
    assert Quant.eltsLtIndex(new int[] {0}) == false;
    assert Quant.eltsLtIndex(new int[] {1}) == false;
    assert Quant.eltsLtIndex(new int[] {-1}) == true;
    assert Quant.eltsLtIndex(new int[] {-1, -1, 1}) == true;
    assert Quant.eltsLtIndex(new int[] {-1, 0, 1}) == true;
    assert Quant.eltsLtIndex(new int[] {-1, 1, 1}) == false;
    assert Quant.eltsLtIndex(new int[] {0, 0, 2}) == false;
    assert Quant.eltsLtIndex(new int[] {0, 1, 2}) == false;
    assert Quant.eltsLtIndex(new int[] {0, 2, 2}) == false;
    assert Quant.eltsLtIndex(new int[] {1, 1, 3}) == false;
    assert Quant.eltsLtIndex(new int[] {1, 2, 3}) == false;
    assert Quant.eltsLtIndex(new int[] {1, 3, 3}) == false;
  }

  public static void test_lexEqual() {
    assert Quant.lexEqual(new int[] {}, new int[] {}) == true;
    assert Quant.lexEqual(new int[] {1}, new int[] {}) == false;
    assert Quant.lexEqual(new int[] {}, new int[] {1}) == false;
    assert Quant.lexEqual(new int[] {1}, new int[] {1}) == true;
    assert Quant.lexEqual(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.lexEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.lexEqual(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == false;
    assert Quant.lexEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.lexEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.lexEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_lexNotEqual() {
    assert Quant.lexNotEqual(new int[] {}, new int[] {}) == false;
    assert Quant.lexNotEqual(new int[] {1}, new int[] {}) == true;
    assert Quant.lexNotEqual(new int[] {}, new int[] {1}) == true;
    assert Quant.lexNotEqual(new int[] {1}, new int[] {1}) == false;
    assert Quant.lexNotEqual(new int[] {1, 2}, new int[] {1, 2, 3}) == true;
    assert Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == true;
    assert Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == true;
    assert Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
  }

  public static void test_lexLT() {
    assert Quant.lexLT(new int[] {}, new int[] {}) == false;
    assert Quant.lexLT(new int[] {1}, new int[] {}) == false;
    assert Quant.lexLT(new int[] {}, new int[] {1}) == true;
    assert Quant.lexLT(new int[] {1}, new int[] {1}) == false;
    assert Quant.lexLT(new int[] {1, 2}, new int[] {1, 2, 3}) == true;
    assert Quant.lexLT(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.lexLT(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == true;
    assert Quant.lexLT(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.lexLT(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.lexLT(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
  }

  public static void test_lexLTE() {
    assert Quant.lexLTE(new int[] {}, new int[] {}) == true;
    assert Quant.lexLTE(new int[] {1}, new int[] {}) == false;
    assert Quant.lexLTE(new int[] {}, new int[] {1}) == true;
    assert Quant.lexLTE(new int[] {1}, new int[] {1}) == true;
    assert Quant.lexLTE(new int[] {1, 2}, new int[] {1, 2, 3}) == true;
    assert Quant.lexLTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.lexLTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == true;
    assert Quant.lexLTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == false;
    assert Quant.lexLTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == true;
    assert Quant.lexLTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == true;
  }

  public static void test_lexGT() {
    assert Quant.lexGT(new int[] {}, new int[] {}) == false;
    assert Quant.lexGT(new int[] {1}, new int[] {}) == true;
    assert Quant.lexGT(new int[] {}, new int[] {1}) == false;
    assert Quant.lexGT(new int[] {1}, new int[] {1}) == false;
    assert Quant.lexGT(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.lexGT(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == false;
    assert Quant.lexGT(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == false;
    assert Quant.lexGT(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == true;
    assert Quant.lexGT(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.lexGT(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_lexGTE() {
    assert Quant.lexGTE(new int[] {}, new int[] {}) == true;
    assert Quant.lexGTE(new int[] {1}, new int[] {}) == true;
    assert Quant.lexGTE(new int[] {}, new int[] {1}) == false;
    assert Quant.lexGTE(new int[] {1}, new int[] {1}) == true;
    assert Quant.lexGTE(new int[] {1, 2}, new int[] {1, 2, 3}) == false;
    assert Quant.lexGTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}) == true;
    assert Quant.lexGTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}) == false;
    assert Quant.lexGTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}) == true;
    assert Quant.lexGTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}) == false;
    assert Quant.lexGTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}) == false;
  }

  public static void test_pairwiseDivides() {
    assert Quant.pairwiseDivides(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseDivides(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseDivides(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseDivides(new int[] {1}, new int[] {1}) == true;
    assert Quant.pairwiseDivides(new int[] {4}, new int[] {2}) == true;
    assert Quant.pairwiseDivides(new int[] {27}, new int[] {3}) == true;
    assert Quant.pairwiseDivides(new int[] {27}, new int[] {4}) == false;
    assert Quant.pairwiseDivides(new int[] {1, 6, 6}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseDivides(new int[] {1, -6, 6}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseDivides(new int[] {1, 6, 7}, new int[] {1, 2, 3}) == false;
  }

  public static void test_pairwiseSquare() {
    assert Quant.pairwiseSquare(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseSquare(new int[] {}, new int[] {1}) == false;
    assert Quant.pairwiseSquare(new int[] {1}, new int[] {}) == false;
    assert Quant.pairwiseSquare(new int[] {1}, new int[] {1}) == true;
    assert Quant.pairwiseSquare(new int[] {4}, new int[] {2}) == true;
    assert Quant.pairwiseSquare(new int[] {27}, new int[] {3}) == false;
    assert Quant.pairwiseSquare(new int[] {27}, new int[] {4}) == false;
    assert Quant.pairwiseSquare(new int[] {1, 4, 9}, new int[] {1, 2, 3}) == true;
    assert Quant.pairwiseSquare(new int[] {1, -4, 9}, new int[] {1, 2, 3}) == false;
    assert Quant.pairwiseSquare(new int[] {1, 4, 10}, new int[] {1, 2, 3}) == false;
  }

  public static void test_pairwiseBitwiseComplement() {
    assert Quant.pairwiseBitwiseComplement(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseBitwiseComplement(new int[] {3, 3}, new int[] {-4, -4}) == true;
    assert Quant.pairwiseBitwiseComplement(new int[] {3, 3}, new int[] {3, -4}) == false;
    assert Quant.pairwiseBitwiseComplement(new int[] {3, 21, 0}, new int[] {-4, -22, -1}) == true;
  }

  public static void test_pairwiseBitwiseSubset() {
    assert Quant.pairwiseBitwiseSubset(new int[] {}, new int[] {}) == true;
    assert Quant.pairwiseBitwiseSubset(new int[] {5, 5}, new int[] {4, 1}) == true;
    assert Quant.pairwiseBitwiseSubset(new int[] {5, 5}, new int[] {4, 3}) == false;
  }

  public static class Foo1 {
    public Bar1 x;
    public static Bar1 xstatic;
  }

  public static class Bar1 {
    private Baz1 y;

    public void set_y(Baz1 o) {
      y = o;
    }

    public Baz1f yf;
  }

  public static class Baz1 {
    public int[] z;
  }

  public static class Baz1f {
    public int z;
  }

  public static class Foo2 {
    public Object[] x;
    private static Object xstatic;

    public static void set_xstatic(Object o) {
      xstatic = o;
    }
  }

  public static class Foo2f {
    private Object x;

    public void set_x(Object o) {
      x = o;
    }
  }

  public static class Foo3 {
    private Bar3[] x;

    public void set_x(Bar3[] o) {
      x = o;
    }
  }

  public static class Foo3f {
    public Bar3 x;
  }

  public static class Bar3 {
    public Baz3 y;
  }

  public static class Baz3 {
    public int z;
  }

  public static class Foo3a {
    private java.util.List<Bar3a> x;

    public void set_x(java.util.List<Bar3a> o) {
      x = o;
    }
  }

  public static class Foo3af {
    public Bar3a x;
  }

  public static class Bar3a {
    public Baz3a y;
  }

  public static class Baz3a {
    public int z;
  }

  public static class Foo4 {
    public Bar4 x;
  }

  public static class Bar4 {
    private Baz4[] y;

    public void set_y(Baz4[] o) {
      y = o;
    }
  }

  public static class Bar4f {
    public Baz4 y;
  }

  public static class Baz4 {
    public String z;
  }

  public static void testCollect() {

    Foo1 f1 = new Foo1();
    f1.x = new Bar1();
    f1.x.y = new Baz1();
    f1.x.y.z = new int[] {1, 2, 3, 4};
    assert_arrays_equals(Quant.collectint(f1, "x.y.z"), new int[] {1, 2, 3, 4});

    Foo2 f2 = new Foo2();
    f2.x = new Object[] {null, "hi", "foobar"};
    Object[] a1 = Quant.collectObject(f2, "x");
    assert a1[0] == null && a1[1].equals("hi") && a1[2].equals("foobar");

    Bar3 b3 = new Bar3();
    b3.y = new Baz3();
    b3.y.z = 7;
    Bar3 b4 = new Bar3();
    b4.y = new Baz3();
    b4.y.z = 8;
    Bar3 b5 = new Bar3();
    b5.y = new Baz3();
    b5.y.z = 9;
    Foo3 f3 = new Foo3();
    f3.x = new Bar3[] {b3, b4, b5};
    assert_arrays_equals(Quant.collectint(f3, "x.y.z"), new int[] {7, 8, 9});

    Bar3a b3a = new Bar3a();
    b3a.y = new Baz3a();
    b3a.y.z = 7;
    Bar3a b4a = new Bar3a();
    b4a.y = new Baz3a();
    b4a.y.z = 8;
    Bar3a b5a = new Bar3a();
    b5a.y = new Baz3a();
    b5a.y.z = 9;
    Foo3a f3a = new Foo3a();
    f3a.x = new java.util.ArrayList<Bar3a>();
    f3a.x.add(b3a);
    f3a.x.add(b4a);
    f3a.x.add(b5a);
    assert_arrays_equals(Quant.collectint(f3a, "x.y.z"), new int[] {7, 8, 9});

    Baz4 z1 = new Baz4();
    z1.z = "hi1";
    Baz4 z2 = new Baz4();
    z2.z = "hi2";
    Foo4 f4 = new Foo4();
    f4.x = new Bar4();
    f4.x.y = new Baz4[] {z1, z2};
    String[] a3 = Quant.collectString(f4, "x.y.z");
    String[] a4 = new String[] {"hi1", "hi2"};
    assert a3[0].equals(a4[0]) && a3[1].equals(a4[1]);
  }

  public static void testCollect_field() {

    Foo1 f1 = new Foo1();
    f1.x = new Bar1();
    f1.x.yf = new Baz1f();
    f1.x.yf.z = 7;

    assert Quant.collectint_field(f1, "x.yf.z") == 7;

    Foo2f f2 = new Foo2f();
    f2.x = "hi";
    Object a1 = Quant.collectObject_field(f2, "x");
    assert a1.equals("hi");

    Bar3 b3 = new Bar3();
    b3.y = new Baz3();
    b3.y.z = 7;
    Bar3 b4 = new Bar3();
    b4.y = new Baz3();
    b4.y.z = 8;
    Bar3 b5 = new Bar3();
    b5.y = new Baz3();
    b5.y.z = 9;
    assert Quant.collectint_field(b3, "y.z") == 7;
    assert Quant.collectint_field(b4, "y.z") == 8;
    assert Quant.collectint_field(b5, "y.z") == 9;

    Baz4 z1 = new Baz4();
    z1.z = "hi1";
    Baz4 z2 = new Baz4();
    z2.z = "hi2";
    assert Quant.collectString_field(z1, "z").equals("hi1");
    assert Quant.collectString_field(z2, "z").equals("hi2");

    Foo1.xstatic = new Bar1();
    Foo1.xstatic.yf = new Baz1f();
    Foo1.xstatic.yf.z = 7;

    assert Quant.collectint_field(Foo1.class, "xstatic.yf.z") == 7;

    Foo2.set_xstatic("hi");
    a1 = Quant.collectObject_field(Foo2.class, "xstatic");
    assert a1.equals("hi");
  }
}
