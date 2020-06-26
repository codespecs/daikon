package daikon.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import daikon.*;
import java.util.Arrays;
import junit.framework.*;
import org.junit.Test;

@SuppressWarnings({"nullness", "UnusedVariable"}) // testing code
public final class TestQuant {

  public static final void assert_arrays_equals(int[] a1, int[] a2) {
    boolean result = Arrays.equals(a1, a2);
    if (!result) {
      System.out.println("Arrays differ: " + Arrays.toString(a1) + ", " + Arrays.toString(a2));
    }
    assertTrue(result);
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
  //     assertEquals(1, Quant.min(new int[] { 1,2,3 }));
  //     assertEquals(1, Quant.min(new int[] { 2,33,1 }));
  //     assertEquals(-2, Quant.min(new int[] { 3,-2,1 }));
  //     assertEquals(3, Quant.min(new int[] { 3 }));
  //   }

  //   public static void test_max() {
  //     assertEquals(3, Quant.max(new int[] { 1,2,3 }));
  //     assertEquals(33, Quant.max(new int[] { 2,33,1 }));
  //     assertEquals(3, Quant.max(new int[] { 3,-2,1 }));
  //     assertEquals(3, Quant.max(new int[] { 3 }));
  //   }

  @Test
  public void test_concat() {
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

  @Test
  public void test_union() {
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

  @Test
  public void test_intersection() {
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

  @Test
  public void test_setDiff() {
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

  @Test
  public void test_setEqual() {
    assertTrue(Quant.setEqual(new int[] {}, new int[] {}));
    assertFalse(Quant.setEqual(new int[] {1}, new int[] {}));
    assertFalse(Quant.setEqual(new int[] {}, new int[] {1}));
    assertTrue(Quant.setEqual(new int[] {1}, new int[] {1}));
    assertTrue(Quant.setEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.setEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.setEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
    assertFalse(Quant.setEqual(new int[] {1, 2, 3}, new int[] {3, 4, 5}));
    assertFalse(
        Quant.setEqual(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5}));
    assertFalse(Quant.setEqual(new int[] {-1, 2, 3, 4}, new int[] {5}));
  }

  @Test
  public void test_subsetOf() {
    assertTrue(Quant.subsetOf(new int[] {-1}, new int[] {-1, 1}));
    assertFalse(Quant.subsetOf(new int[] {-1, 0, 1}, new int[] {-1, 1}));
    assertTrue(Quant.subsetOf(new int[] {-1, 1}, new int[] {-1, 1}));
    assertFalse(Quant.subsetOf(new int[] {-1, 2, 3, 4}, new int[] {5}));
    assertFalse(
        Quant.subsetOf(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5}));
    assertTrue(Quant.subsetOf(new int[] {1}, new int[] {-1, 1}));
    assertTrue(Quant.subsetOf(new int[] {1}, new int[] {1}));
    assertTrue(Quant.subsetOf(new int[] {1}, new int[] {1, 2, 3}));
    assertFalse(Quant.subsetOf(new int[] {1}, new int[] {}));
    assertFalse(Quant.subsetOf(new int[] {1, 2}, new int[] {1}));
    assertTrue(Quant.subsetOf(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.subsetOf(new int[] {1, 2}, new int[] {}));
    assertTrue(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2}));
    assertFalse(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2, 3}));
    assertTrue(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
    assertFalse(Quant.subsetOf(new int[] {1, 2, 3}, new int[] {3, 4, 5}));
    assertTrue(Quant.subsetOf(new int[] {2, 1}, new int[] {1, 2, 3}));
    assertFalse(Quant.subsetOf(new int[] {2, 3}, new int[] {3, 3, 3, 3}));
    assertTrue(Quant.subsetOf(new int[] {3, 1}, new int[] {1, 2, 3}));
    assertTrue(Quant.subsetOf(new int[] {3, 3}, new int[] {3, 3, 3, 3}));
    assertTrue(Quant.subsetOf(new int[] {}, new int[] {-1, 1}));
    assertTrue(Quant.subsetOf(new int[] {}, new int[] {1}));
    assertTrue(Quant.subsetOf(new int[] {}, new int[] {2, 3, 1}));
    assertTrue(Quant.subsetOf(new int[] {}, new int[] {}));
  }

  @Test
  public void test_subsetOf_different_types() {
    assertTrue(Quant.subsetOf(new byte[] {}, new int[] {}));
    assertTrue(Quant.subsetOf(new long[] {}, new long[] {}));
    assertFalse(Quant.subsetOf(new byte[] {1}, new long[] {}));
    assertFalse(Quant.subsetOf(new short[] {1, 2}, new short[] {}));
    assertTrue(Quant.subsetOf(new long[] {}, new short[] {1}));
    assertTrue(Quant.subsetOf(new int[] {1}, new short[] {1}));
    assertFalse(Quant.subsetOf(new float[] {1, 2}, new double[] {1}));
    assertTrue(Quant.subsetOf(new double[] {}, new double[] {-1, 1}));
    assertTrue(Quant.subsetOf(new float[] {1}, new float[] {-1, 1}));
    assertTrue(Quant.subsetOf(new double[] {-1}, new float[] {-1, 1}));
    assertTrue(Quant.subsetOf(new byte[] {-1, 1}, new short[] {-1, 1}));
    assertFalse(Quant.subsetOf(new short[] {-1, 0, 1}, new byte[] {-1, 1}));
    assertTrue(Quant.subsetOf(new int[] {1, 2}, new byte[] {1, 2, 3}));
    assertTrue(Quant.subsetOf(new long[] {}, new short[] {2, 3, 1}));
  }

  @Test
  public void test_isReverse() {
    assertTrue(Quant.isReverse(new int[] {}, new int[] {}));
    assertFalse(Quant.isReverse(new int[] {1}, new int[] {}));
    assertFalse(Quant.isReverse(new int[] {}, new int[] {1}));
    assertTrue(Quant.isReverse(new int[] {1}, new int[] {1}));
    assertFalse(Quant.isReverse(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.isReverse(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.isReverse(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
    assertFalse(Quant.isReverse(new int[] {1, 2, 3}, new int[] {3, 4, 5}));
    assertFalse(
        Quant.isReverse(
            new int[] {
              -1,
            },
            new int[] {2, 3, 4, 5}));
    assertFalse(Quant.isReverse(new int[] {-1, 2, 3, 4}, new int[] {5}));
  }

  @Test
  public void test_noDups() {
    assertTrue(Quant.noDups(new int[] {}));
    assertTrue(Quant.noDups(new int[] {1}));
    assertTrue(Quant.noDups(new int[] {-1}));
    assertTrue(Quant.noDups(new int[] {-1, 1}));
    assertTrue(Quant.noDups(new int[] {1, 2, 3}));
    assertTrue(Quant.noDups(new int[] {2, 3, 1}));
    assertFalse(Quant.noDups(new int[] {2, 3, 2, 3}));
    assertFalse(Quant.noDups(new int[] {2, 3, 3, 2}));
    assertFalse(Quant.noDups(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.noDups(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.noDups(new int[] {1, 1, 1, -1}));
    assertTrue(Quant.noDups(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_memberOf() {
    assertFalse(Quant.memberOf(1, new int[] {}));
    assertTrue(Quant.memberOf(1, new int[] {1}));
    assertFalse(Quant.memberOf(-1, new int[] {1}));
    assertFalse(Quant.memberOf(1, new int[] {-1}));
    assertTrue(Quant.memberOf(-1, new int[] {-1}));
    assertTrue(Quant.memberOf(1, new int[] {-1, 1}));
    assertTrue(Quant.memberOf(-1, new int[] {-1, 1}));
    assertFalse(Quant.memberOf(2, new int[] {-1, 1}));
    assertTrue(Quant.memberOf(1, new int[] {1, 2, 3}));
    assertTrue(Quant.memberOf(2, new int[] {1, 2, 3}));
    assertTrue(Quant.memberOf(3, new int[] {1, 2, 3}));
    assertFalse(Quant.memberOf(4, new int[] {1, 2, 3}));
    assertTrue(Quant.memberOf(1, new int[] {2, 3, 1}));
    assertTrue(Quant.memberOf(2, new int[] {2, 3, 1}));
    assertTrue(Quant.memberOf(3, new int[] {2, 3, 1}));
    assertFalse(Quant.memberOf(4, new int[] {2, 3, 1}));
    assertTrue(Quant.memberOf(3, new int[] {3, 3, 3, 3}));
    assertFalse(Quant.memberOf(2, new int[] {3, 3, 3, 3}));
    assertTrue(Quant.memberOf(-1, new int[] {-1, 1, 2, 3, 4, 5, 6}));
    assertTrue(Quant.memberOf(1, new int[] {-1, 1, 2, 3, 4, 5, 6}));
    assertTrue(Quant.memberOf(6, new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_slice() {
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

  @Test
  public void test_eltsEqual() {
    assertTrue(Quant.eltsEqual(new int[] {}, 0));
    assertFalse(Quant.eltsEqual(new int[] {1}, -1));
    assertTrue(Quant.eltsEqual(new int[] {1}, 1));
    assertFalse(Quant.eltsEqual(new int[] {-1}, 1));
    assertTrue(Quant.eltsEqual(new int[] {-1}, -1));
    assertFalse(Quant.eltsEqual(new int[] {-1, 1}, 1));
    assertFalse(Quant.eltsEqual(new int[] {-1, 1}, -1));
    assertFalse(Quant.eltsEqual(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsEqual(new int[] {1, 2, 3}, 2));
    assertFalse(Quant.eltsEqual(new int[] {1, 2, 3}, 3));
    assertFalse(Quant.eltsEqual(new int[] {2, 3, 2, 3}, 2));
    assertFalse(Quant.eltsEqual(new int[] {2, 3, 2, 3}, 3));
    assertTrue(Quant.eltsEqual(new int[] {3, 3, 3, 3}, 3));
    assertFalse(Quant.eltsEqual(new int[] {1, 1, 1, -1}, 1));
    assertTrue(Quant.eltsEqual(new int[] {1, 1, 1, 1}, 1));
  }

  @Test
  public void test_eltsNotEqual() {
    assertTrue(Quant.eltsNotEqual(new int[] {}, 0));
    assertTrue(Quant.eltsNotEqual(new int[] {1}, -1));
    assertFalse(Quant.eltsNotEqual(new int[] {1}, 1));
    assertTrue(Quant.eltsNotEqual(new int[] {-1}, 1));
    assertFalse(Quant.eltsNotEqual(new int[] {-1}, -1));
    assertFalse(Quant.eltsNotEqual(new int[] {-1, 1}, 1));
    assertFalse(Quant.eltsNotEqual(new int[] {-1, 1}, -1));
    assertFalse(Quant.eltsNotEqual(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsNotEqual(new int[] {1, 2, 3}, 2));
    assertFalse(Quant.eltsNotEqual(new int[] {1, 2, 3}, 3));
    assertFalse(Quant.eltsNotEqual(new int[] {2, 3, 2, 3}, 2));
    assertFalse(Quant.eltsNotEqual(new int[] {2, 3, 2, 3}, 3));
    assertFalse(Quant.eltsNotEqual(new int[] {3, 3, 3, 3}, 3));
    assertFalse(Quant.eltsNotEqual(new int[] {1, 1, 1, -1}, 1));
    assertTrue(Quant.eltsNotEqual(new int[] {-1, -1, -1, -1}, 1));
    assertFalse(Quant.eltsNotEqual(new int[] {1, 1, 1, 1}, 1));
  }

  @Test
  public void test_eltsGT() {
    assertTrue(Quant.eltsGT(new int[] {}, 0));
    assertTrue(Quant.eltsGT(new int[] {1}, -1));
    assertFalse(Quant.eltsGT(new int[] {1}, 1));
    assertFalse(Quant.eltsGT(new int[] {-1}, 1));
    assertFalse(Quant.eltsGT(new int[] {-1}, -1));
    assertFalse(Quant.eltsGT(new int[] {-1, 1}, 1));
    assertFalse(Quant.eltsGT(new int[] {-1, 1}, -1));
    assertTrue(Quant.eltsGT(new int[] {1, 2, 3}, 0));
    assertFalse(Quant.eltsGT(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsGT(new int[] {1, 2, 3}, 2));
    assertFalse(Quant.eltsGT(new int[] {1, 2, 3}, 3));
    assertFalse(Quant.eltsGT(new int[] {1, 2, 3}, 4));
    assertTrue(Quant.eltsGT(new int[] {2, 3, 2, 3}, 1));
    assertFalse(Quant.eltsGT(new int[] {2, 3, 2, 3}, 2));
    assertFalse(Quant.eltsGT(new int[] {2, 3, 2, 3}, 3));
    assertTrue(Quant.eltsGT(new int[] {3, 3, 3, 3}, 2));
    assertFalse(Quant.eltsGT(new int[] {3, 3, 3, 3}, 3));
  }

  @Test
  public void test_eltsGTE() {
    assertTrue(Quant.eltsGTE(new int[] {}, 0));
    assertTrue(Quant.eltsGTE(new int[] {1}, -1));
    assertTrue(Quant.eltsGTE(new int[] {1}, 1));
    assertFalse(Quant.eltsGTE(new int[] {-1}, 1));
    assertTrue(Quant.eltsGTE(new int[] {-1}, -1));
    assertFalse(Quant.eltsGTE(new int[] {-1, 1}, 1));
    assertTrue(Quant.eltsGTE(new int[] {-1, 1}, -1));
    assertTrue(Quant.eltsGTE(new int[] {1, 2, 3}, 0));
    assertTrue(Quant.eltsGTE(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsGTE(new int[] {1, 2, 3}, 2));
    assertFalse(Quant.eltsGTE(new int[] {1, 2, 3}, 3));
    assertFalse(Quant.eltsGTE(new int[] {1, 2, 3}, 4));
    assertTrue(Quant.eltsGTE(new int[] {2, 3, 2, 3}, 1));
    assertTrue(Quant.eltsGTE(new int[] {2, 3, 2, 3}, 2));
    assertFalse(Quant.eltsGTE(new int[] {2, 3, 2, 3}, 3));
    assertTrue(Quant.eltsGTE(new int[] {3, 3, 3, 3}, 2));
    assertTrue(Quant.eltsGTE(new int[] {3, 3, 3, 3}, 3));
  }

  @Test
  public void test_eltsLT() {
    assertTrue(Quant.eltsLT(new int[] {}, 0));
    assertFalse(Quant.eltsLT(new int[] {1}, -1));
    assertFalse(Quant.eltsLT(new int[] {1}, 1));
    assertTrue(Quant.eltsLT(new int[] {-1}, 1));
    assertFalse(Quant.eltsLT(new int[] {-1}, -1));
    assertFalse(Quant.eltsLT(new int[] {-1, 1}, 1));
    assertFalse(Quant.eltsLT(new int[] {-1, 1}, -1));
    assertFalse(Quant.eltsLT(new int[] {1, 2, 3}, 0));
    assertFalse(Quant.eltsLT(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsLT(new int[] {1, 2, 3}, 2));
    assertFalse(Quant.eltsLT(new int[] {1, 2, 3}, 3));
    assertTrue(Quant.eltsLT(new int[] {1, 2, 3}, 4));
    assertFalse(Quant.eltsLT(new int[] {2, 3, 2, 3}, 1));
    assertFalse(Quant.eltsLT(new int[] {2, 3, 2, 3}, 2));
    assertFalse(Quant.eltsLT(new int[] {2, 3, 2, 3}, 3));
    assertFalse(Quant.eltsLT(new int[] {3, 3, 3, 3}, 2));
    assertFalse(Quant.eltsLT(new int[] {3, 3, 3, 3}, 3));
  }

  @Test
  public void test_eltsLTE() {
    assertTrue(Quant.eltsLTE(new int[] {}, 0));
    assertFalse(Quant.eltsLTE(new int[] {1}, -1));
    assertTrue(Quant.eltsLTE(new int[] {1}, 1));
    assertTrue(Quant.eltsLTE(new int[] {-1}, 1));
    assertTrue(Quant.eltsLTE(new int[] {-1}, -1));
    assertTrue(Quant.eltsLTE(new int[] {-1, 1}, 1));
    assertFalse(Quant.eltsLTE(new int[] {-1, 1}, -1));
    assertFalse(Quant.eltsLTE(new int[] {1, 2, 3}, 0));
    assertFalse(Quant.eltsLTE(new int[] {1, 2, 3}, 1));
    assertFalse(Quant.eltsLTE(new int[] {1, 2, 3}, 2));
    assertTrue(Quant.eltsLTE(new int[] {1, 2, 3}, 3));
    assertTrue(Quant.eltsLTE(new int[] {1, 2, 3}, 4));
    assertFalse(Quant.eltsLTE(new int[] {2, 3, 2, 3}, 1));
    assertFalse(Quant.eltsLTE(new int[] {2, 3, 2, 3}, 2));
    assertTrue(Quant.eltsLTE(new int[] {2, 3, 2, 3}, 3));
    assertFalse(Quant.eltsLTE(new int[] {3, 3, 3, 3}, 2));
    assertTrue(Quant.eltsLTE(new int[] {3, 3, 3, 3}, 3));
  }

  @Test
  public void test_pairwiseEqual() {
    assertTrue(Quant.pairwiseEqual(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseEqual(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseEqual(new int[] {}, new int[] {1}));
    assertTrue(Quant.pairwiseEqual(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseEqual(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseNotEqual() {
    assertTrue(Quant.pairwiseNotEqual(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {}, new int[] {1}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertTrue(Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseNotEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseLT() {
    assertTrue(Quant.pairwiseLT(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseLT(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseLT(new int[] {}, new int[] {1}));
    assertFalse(Quant.pairwiseLT(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseLT(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertFalse(Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseLT(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseLTE() {
    assertTrue(Quant.pairwiseLTE(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseLTE(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseLTE(new int[] {}, new int[] {1}));
    assertTrue(Quant.pairwiseLTE(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseLTE(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertFalse(Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseLTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseGT() {
    assertTrue(Quant.pairwiseGT(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseGT(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseGT(new int[] {}, new int[] {1}));
    assertFalse(Quant.pairwiseGT(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertTrue(Quant.pairwiseGT(new int[] {2, 3, 4}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseGT(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseGTE() {
    assertTrue(Quant.pairwiseGTE(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseGTE(new int[] {1}, new int[] {}));
    assertFalse(Quant.pairwiseGTE(new int[] {}, new int[] {1}));
    assertTrue(Quant.pairwiseGTE(new int[] {1}, new int[] {1}));
    assertFalse(Quant.pairwiseGTE(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertTrue(Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.pairwiseGTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_eltwiseEqual() {
    assertTrue(Quant.eltwiseEqual(new int[] {}));
    assertTrue(Quant.eltwiseEqual(new int[] {1}));
    assertTrue(Quant.eltwiseEqual(new int[] {-1}));
    assertFalse(Quant.eltwiseEqual(new int[] {-1, 1}));
    assertTrue(Quant.eltwiseEqual(new int[] {1, 1}));
    assertFalse(Quant.eltwiseEqual(new int[] {1, 2, 3}));
    assertFalse(Quant.eltwiseEqual(new int[] {2, 3, 1}));
    assertFalse(Quant.eltwiseEqual(new int[] {3, 2, 1}));
    assertFalse(Quant.eltwiseEqual(new int[] {2, 3, 3, 3}));
    assertFalse(Quant.eltwiseEqual(new int[] {2, 3, 3, 4}));
    assertFalse(Quant.eltwiseEqual(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.eltwiseEqual(new int[] {4, 3, 3, 2}));
    assertTrue(Quant.eltwiseEqual(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.eltwiseEqual(new int[] {2, 3, 2, 3}));
    assertFalse(Quant.eltwiseEqual(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltwiseNotEqual() {
    assertTrue(Quant.eltwiseNotEqual(new int[] {}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {1}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {-1}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {-1, 1}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {1, 1}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {1, 2, 3}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {2, 3, 1}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {3, 2, 1}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {2, 3, 3, 3}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {2, 3, 3, 4}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {4, 3, 3, 2}));
    assertFalse(Quant.eltwiseNotEqual(new int[] {3, 3, 3, 3}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {2, 3, 2, 3}));
    assertTrue(Quant.eltwiseNotEqual(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltwiseLT() {
    assertTrue(Quant.eltwiseLT(new int[] {}));
    assertTrue(Quant.eltwiseLT(new int[] {1}));
    assertTrue(Quant.eltwiseLT(new int[] {-1}));
    assertTrue(Quant.eltwiseLT(new int[] {-1, 1}));
    assertFalse(Quant.eltwiseLT(new int[] {1, 1}));
    assertTrue(Quant.eltwiseLT(new int[] {1, 2, 3}));
    assertFalse(Quant.eltwiseLT(new int[] {2, 3, 1}));
    assertFalse(Quant.eltwiseLT(new int[] {3, 2, 1}));
    assertFalse(Quant.eltwiseLT(new int[] {2, 3, 3, 3}));
    assertFalse(Quant.eltwiseLT(new int[] {2, 3, 3, 4}));
    assertFalse(Quant.eltwiseLT(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.eltwiseLT(new int[] {4, 3, 3, 2}));
    assertFalse(Quant.eltwiseLT(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.eltwiseLT(new int[] {2, 3, 2, 3}));
    assertTrue(Quant.eltwiseLT(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltwiseLTE() {
    assertTrue(Quant.eltwiseLTE(new int[] {}));
    assertTrue(Quant.eltwiseLTE(new int[] {1}));
    assertTrue(Quant.eltwiseLTE(new int[] {-1}));
    assertTrue(Quant.eltwiseLTE(new int[] {-1, 1}));
    assertTrue(Quant.eltwiseLTE(new int[] {1, 1}));
    assertTrue(Quant.eltwiseLTE(new int[] {1, 2, 3}));
    assertFalse(Quant.eltwiseLTE(new int[] {2, 3, 1}));
    assertFalse(Quant.eltwiseLTE(new int[] {3, 2, 1}));
    assertTrue(Quant.eltwiseLTE(new int[] {2, 3, 3, 3}));
    assertTrue(Quant.eltwiseLTE(new int[] {2, 3, 3, 4}));
    assertFalse(Quant.eltwiseLTE(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.eltwiseLTE(new int[] {4, 3, 3, 2}));
    assertTrue(Quant.eltwiseLTE(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.eltwiseLTE(new int[] {2, 3, 2, 3}));
    assertTrue(Quant.eltwiseLTE(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltwiseGT() {
    assertTrue(Quant.eltwiseGT(new int[] {}));
    assertTrue(Quant.eltwiseGT(new int[] {1}));
    assertTrue(Quant.eltwiseGT(new int[] {-1}));
    assertFalse(Quant.eltwiseGT(new int[] {-1, 1}));
    assertFalse(Quant.eltwiseGT(new int[] {1, 1}));
    assertFalse(Quant.eltwiseGT(new int[] {1, 2, 3}));
    assertFalse(Quant.eltwiseGT(new int[] {2, 3, 1}));
    assertTrue(Quant.eltwiseGT(new int[] {3, 2, 1}));
    assertFalse(Quant.eltwiseGT(new int[] {2, 3, 3, 3}));
    assertFalse(Quant.eltwiseGT(new int[] {2, 3, 3, 4}));
    assertFalse(Quant.eltwiseGT(new int[] {3, 3, 3, 2}));
    assertFalse(Quant.eltwiseGT(new int[] {4, 3, 3, 2}));
    assertFalse(Quant.eltwiseGT(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.eltwiseGT(new int[] {2, 3, 2, 3}));
    assertFalse(Quant.eltwiseGT(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltwiseGTE() {
    assertTrue(Quant.eltwiseGTE(new int[] {}));
    assertTrue(Quant.eltwiseGTE(new int[] {1}));
    assertTrue(Quant.eltwiseGTE(new int[] {-1}));
    assertFalse(Quant.eltwiseGTE(new int[] {-1, 1}));
    assertTrue(Quant.eltwiseGTE(new int[] {1, 1}));
    assertFalse(Quant.eltwiseGTE(new int[] {1, 2, 3}));
    assertFalse(Quant.eltwiseGTE(new int[] {2, 3, 1}));
    assertTrue(Quant.eltwiseGTE(new int[] {3, 2, 1}));
    assertFalse(Quant.eltwiseGTE(new int[] {2, 3, 3, 3}));
    assertFalse(Quant.eltwiseGTE(new int[] {2, 3, 3, 4}));
    assertTrue(Quant.eltwiseGTE(new int[] {3, 3, 3, 2}));
    assertTrue(Quant.eltwiseGTE(new int[] {4, 3, 3, 2}));
    assertTrue(Quant.eltwiseGTE(new int[] {3, 3, 3, 3}));
    assertFalse(Quant.eltwiseGTE(new int[] {2, 3, 2, 3}));
    assertFalse(Quant.eltwiseGTE(new int[] {-1, 1, 2, 3, 4, 5, 6}));
  }

  @Test
  public void test_eltsEqualIndex() {
    assertTrue(Quant.eltsEqualIndex(new int[] {}));
    assertTrue(Quant.eltsEqualIndex(new int[] {0}));
    assertFalse(Quant.eltsEqualIndex(new int[] {1}));
    assertFalse(Quant.eltsEqualIndex(new int[] {-1}));
    assertFalse(Quant.eltsEqualIndex(new int[] {-1, -1, 1}));
    assertFalse(Quant.eltsEqualIndex(new int[] {-1, 0, 1}));
    assertFalse(Quant.eltsEqualIndex(new int[] {-1, 1, 1}));
    assertFalse(Quant.eltsEqualIndex(new int[] {0, 0, 2}));
    assertTrue(Quant.eltsEqualIndex(new int[] {0, 1, 2}));
    assertFalse(Quant.eltsEqualIndex(new int[] {0, 2, 2}));
    assertFalse(Quant.eltsEqualIndex(new int[] {1, 1, 3}));
    assertFalse(Quant.eltsEqualIndex(new int[] {1, 2, 3}));
    assertFalse(Quant.eltsEqualIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_eltsNotEqualIndex() {
    assertTrue(Quant.eltsNotEqualIndex(new int[] {}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {0}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {1}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {-1}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {-1, -1, 1}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {-1, 0, 1}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {-1, 1, 1}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {0, 0, 2}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {0, 1, 2}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {0, 2, 2}));
    assertFalse(Quant.eltsNotEqualIndex(new int[] {1, 1, 3}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {1, 2, 3}));
    assertTrue(Quant.eltsNotEqualIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_eltsGteIndex() {
    assertTrue(Quant.eltsGteIndex(new int[] {}));
    assertTrue(Quant.eltsGteIndex(new int[] {0}));
    assertTrue(Quant.eltsGteIndex(new int[] {1}));
    assertFalse(Quant.eltsGteIndex(new int[] {-1}));
    assertFalse(Quant.eltsGteIndex(new int[] {-1, -1, 1}));
    assertFalse(Quant.eltsGteIndex(new int[] {-1, 0, 1}));
    assertFalse(Quant.eltsGteIndex(new int[] {-1, 1, 1}));
    assertFalse(Quant.eltsGteIndex(new int[] {0, 0, 2}));
    assertTrue(Quant.eltsGteIndex(new int[] {0, 1, 2}));
    assertTrue(Quant.eltsGteIndex(new int[] {0, 2, 2}));
    assertTrue(Quant.eltsGteIndex(new int[] {1, 1, 3}));
    assertTrue(Quant.eltsGteIndex(new int[] {1, 2, 3}));
    assertTrue(Quant.eltsGteIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_eltsGtIndex() {
    assertTrue(Quant.eltsGtIndex(new int[] {}));
    assertFalse(Quant.eltsGtIndex(new int[] {0}));
    assertTrue(Quant.eltsGtIndex(new int[] {1}));
    assertFalse(Quant.eltsGtIndex(new int[] {-1}));
    assertFalse(Quant.eltsGtIndex(new int[] {-1, -1, 1}));
    assertFalse(Quant.eltsGtIndex(new int[] {-1, 0, 1}));
    assertFalse(Quant.eltsGtIndex(new int[] {-1, 1, 1}));
    assertFalse(Quant.eltsGtIndex(new int[] {0, 0, 2}));
    assertFalse(Quant.eltsGtIndex(new int[] {0, 1, 2}));
    assertFalse(Quant.eltsGtIndex(new int[] {0, 2, 2}));
    assertFalse(Quant.eltsGtIndex(new int[] {1, 1, 3}));
    assertTrue(Quant.eltsGtIndex(new int[] {1, 2, 3}));
    assertTrue(Quant.eltsGtIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_eltsLteIndex() {
    assertTrue(Quant.eltsLteIndex(new int[] {}));
    assertTrue(Quant.eltsLteIndex(new int[] {0}));
    assertFalse(Quant.eltsLteIndex(new int[] {1}));
    assertTrue(Quant.eltsLteIndex(new int[] {-1}));
    assertTrue(Quant.eltsLteIndex(new int[] {-1, -1, 1}));
    assertTrue(Quant.eltsLteIndex(new int[] {-1, 0, 1}));
    assertTrue(Quant.eltsLteIndex(new int[] {-1, 1, 1}));
    assertTrue(Quant.eltsLteIndex(new int[] {0, 0, 2}));
    assertTrue(Quant.eltsLteIndex(new int[] {0, 1, 2}));
    assertFalse(Quant.eltsLteIndex(new int[] {0, 2, 2}));
    assertFalse(Quant.eltsLteIndex(new int[] {1, 1, 3}));
    assertFalse(Quant.eltsLteIndex(new int[] {1, 2, 3}));
    assertFalse(Quant.eltsLteIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_eltsLtIndex() {
    assertTrue(Quant.eltsLtIndex(new int[] {}));
    assertFalse(Quant.eltsLtIndex(new int[] {0}));
    assertFalse(Quant.eltsLtIndex(new int[] {1}));
    assertTrue(Quant.eltsLtIndex(new int[] {-1}));
    assertTrue(Quant.eltsLtIndex(new int[] {-1, -1, 1}));
    assertTrue(Quant.eltsLtIndex(new int[] {-1, 0, 1}));
    assertFalse(Quant.eltsLtIndex(new int[] {-1, 1, 1}));
    assertFalse(Quant.eltsLtIndex(new int[] {0, 0, 2}));
    assertFalse(Quant.eltsLtIndex(new int[] {0, 1, 2}));
    assertFalse(Quant.eltsLtIndex(new int[] {0, 2, 2}));
    assertFalse(Quant.eltsLtIndex(new int[] {1, 1, 3}));
    assertFalse(Quant.eltsLtIndex(new int[] {1, 2, 3}));
    assertFalse(Quant.eltsLtIndex(new int[] {1, 3, 3}));
  }

  @Test
  public void test_lexEqual() {
    assertTrue(Quant.lexEqual(new int[] {}, new int[] {}));
    assertFalse(Quant.lexEqual(new int[] {1}, new int[] {}));
    assertFalse(Quant.lexEqual(new int[] {}, new int[] {1}));
    assertTrue(Quant.lexEqual(new int[] {1}, new int[] {1}));
    assertFalse(Quant.lexEqual(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexEqual(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertFalse(Quant.lexEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.lexEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.lexEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_lexNotEqual() {
    assertFalse(Quant.lexNotEqual(new int[] {}, new int[] {}));
    assertTrue(Quant.lexNotEqual(new int[] {1}, new int[] {}));
    assertTrue(Quant.lexNotEqual(new int[] {}, new int[] {1}));
    assertFalse(Quant.lexNotEqual(new int[] {1}, new int[] {1}));
    assertTrue(Quant.lexNotEqual(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertTrue(Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertTrue(Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.lexNotEqual(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_lexLT() {
    assertFalse(Quant.lexLT(new int[] {}, new int[] {}));
    assertFalse(Quant.lexLT(new int[] {1}, new int[] {}));
    assertTrue(Quant.lexLT(new int[] {}, new int[] {1}));
    assertFalse(Quant.lexLT(new int[] {1}, new int[] {1}));
    assertTrue(Quant.lexLT(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexLT(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexLT(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertFalse(Quant.lexLT(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertTrue(Quant.lexLT(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.lexLT(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_lexLTE() {
    assertTrue(Quant.lexLTE(new int[] {}, new int[] {}));
    assertFalse(Quant.lexLTE(new int[] {1}, new int[] {}));
    assertTrue(Quant.lexLTE(new int[] {}, new int[] {1}));
    assertTrue(Quant.lexLTE(new int[] {1}, new int[] {1}));
    assertTrue(Quant.lexLTE(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexLTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexLTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertFalse(Quant.lexLTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertTrue(Quant.lexLTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertTrue(Quant.lexLTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_lexGT() {
    assertFalse(Quant.lexGT(new int[] {}, new int[] {}));
    assertTrue(Quant.lexGT(new int[] {1}, new int[] {}));
    assertFalse(Quant.lexGT(new int[] {}, new int[] {1}));
    assertFalse(Quant.lexGT(new int[] {1}, new int[] {1}));
    assertFalse(Quant.lexGT(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexGT(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexGT(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertTrue(Quant.lexGT(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.lexGT(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.lexGT(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_lexGTE() {
    assertTrue(Quant.lexGTE(new int[] {}, new int[] {}));
    assertTrue(Quant.lexGTE(new int[] {1}, new int[] {}));
    assertFalse(Quant.lexGTE(new int[] {}, new int[] {1}));
    assertTrue(Quant.lexGTE(new int[] {1}, new int[] {1}));
    assertFalse(Quant.lexGTE(new int[] {1, 2}, new int[] {1, 2, 3}));
    assertTrue(Quant.lexGTE(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
    assertFalse(Quant.lexGTE(new int[] {1, 2, 3}, new int[] {2, 3, 4}));
    assertTrue(Quant.lexGTE(new int[] {1, 2, 3}, new int[] {1, 2, 2}));
    assertFalse(Quant.lexGTE(new int[] {1, 2, 3}, new int[] {2, 3, 1}));
    assertFalse(Quant.lexGTE(new int[] {1, 2, 3}, new int[] {3, 2, 1}));
  }

  @Test
  public void test_pairwiseDivides() {
    assertTrue(Quant.pairwiseDivides(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseDivides(new int[] {}, new int[] {1}));
    assertFalse(Quant.pairwiseDivides(new int[] {1}, new int[] {}));
    assertTrue(Quant.pairwiseDivides(new int[] {1}, new int[] {1}));
    assertTrue(Quant.pairwiseDivides(new int[] {4}, new int[] {2}));
    assertTrue(Quant.pairwiseDivides(new int[] {27}, new int[] {3}));
    assertFalse(Quant.pairwiseDivides(new int[] {27}, new int[] {4}));
    assertTrue(Quant.pairwiseDivides(new int[] {1, 6, 6}, new int[] {1, 2, 3}));
    assertTrue(Quant.pairwiseDivides(new int[] {1, -6, 6}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseDivides(new int[] {1, 6, 7}, new int[] {1, 2, 3}));
  }

  @Test
  public void test_pairwiseSquare() {
    assertTrue(Quant.pairwiseSquare(new int[] {}, new int[] {}));
    assertFalse(Quant.pairwiseSquare(new int[] {}, new int[] {1}));
    assertFalse(Quant.pairwiseSquare(new int[] {1}, new int[] {}));
    assertTrue(Quant.pairwiseSquare(new int[] {1}, new int[] {1}));
    assertTrue(Quant.pairwiseSquare(new int[] {4}, new int[] {2}));
    assertFalse(Quant.pairwiseSquare(new int[] {27}, new int[] {3}));
    assertFalse(Quant.pairwiseSquare(new int[] {27}, new int[] {4}));
    assertTrue(Quant.pairwiseSquare(new int[] {1, 4, 9}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseSquare(new int[] {1, -4, 9}, new int[] {1, 2, 3}));
    assertFalse(Quant.pairwiseSquare(new int[] {1, 4, 10}, new int[] {1, 2, 3}));
  }

  @Test
  public void test_pairwiseBitwiseComplement() {
    assertTrue(Quant.pairwiseBitwiseComplement(new int[] {}, new int[] {}));
    assertTrue(Quant.pairwiseBitwiseComplement(new int[] {3, 3}, new int[] {-4, -4}));
    assertFalse(Quant.pairwiseBitwiseComplement(new int[] {3, 3}, new int[] {3, -4}));
    assertTrue(Quant.pairwiseBitwiseComplement(new int[] {3, 21, 0}, new int[] {-4, -22, -1}));
  }

  @Test
  public void test_pairwiseBitwiseSubset() {
    assertTrue(Quant.pairwiseBitwiseSubset(new int[] {}, new int[] {}));
    assertTrue(Quant.pairwiseBitwiseSubset(new int[] {5, 5}, new int[] {4, 1}));
    assertFalse(Quant.pairwiseBitwiseSubset(new int[] {5, 5}, new int[] {4, 3}));
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

  @Test
  public void testCollect() {

    Foo1 f1 = new Foo1();
    f1.x = new Bar1();
    f1.x.y = new Baz1();
    f1.x.y.z = new int[] {1, 2, 3, 4};
    assert_arrays_equals(Quant.collectint(f1, "x.y.z"), new int[] {1, 2, 3, 4});

    Foo2 f2 = new Foo2();
    f2.x = new Object[] {null, "hi", "foobar"};
    Object[] a1 = Quant.collectObject(f2, "x");
    assertNull(a1[0]);
    assertEquals("hi", a1[1]);
    assertEquals("foobar", a1[2]);

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
    assertEquals(a4[0], a3[0]);
    assertEquals(a4[1], a3[1]);
  }

  @Test
  public void testCollect_field() {

    Foo1 f1 = new Foo1();
    f1.x = new Bar1();
    f1.x.yf = new Baz1f();
    f1.x.yf.z = 7;

    assertEquals(7, Quant.collectint_field(f1, "x.yf.z"));

    Foo2f f2 = new Foo2f();
    f2.x = "hi";
    Object a1 = Quant.collectObject_field(f2, "x");
    assertEquals("hi", a1);

    Bar3 b3 = new Bar3();
    b3.y = new Baz3();
    b3.y.z = 7;
    Bar3 b4 = new Bar3();
    b4.y = new Baz3();
    b4.y.z = 8;
    Bar3 b5 = new Bar3();
    b5.y = new Baz3();
    b5.y.z = 9;
    assertEquals(7, Quant.collectint_field(b3, "y.z"));
    assertEquals(8, Quant.collectint_field(b4, "y.z"));
    assertEquals(9, Quant.collectint_field(b5, "y.z"));

    Baz4 z1 = new Baz4();
    z1.z = "hi1";
    Baz4 z2 = new Baz4();
    z2.z = "hi2";
    assertEquals("hi1", Quant.collectString_field(z1, "z"));
    assertEquals("hi2", Quant.collectString_field(z2, "z"));

    Foo1.xstatic = new Bar1();
    Foo1.xstatic.yf = new Baz1f();
    Foo1.xstatic.yf.z = 7;

    assertEquals(7, Quant.collectint_field(Foo1.class, "xstatic.yf.z"));

    Foo2.set_xstatic("hi");
    a1 = Quant.collectObject_field(Foo2.class, "xstatic");
    assertEquals("hi", a1);
  }
}
