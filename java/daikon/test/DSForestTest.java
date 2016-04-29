package daikon.test;

import daikon.asm.DSForest;
import java.io.*;
import java.util.*;
import junit.framework.TestCase;

@SuppressWarnings("nullness") // testing code
public class DSForestTest extends TestCase {

  public static final String s1 = "s1";
  public static final String s2 = "s2";
  public static final String s3 = "s3";
  public static final String s4 = "s4";
  public static final String s5 = "s5";
  public static final String s6 = "s6";
  public static final String s7 = "s7";
  public static final String s8 = "s8";
  public static final String s9 = "s9";

  // Test proper argument checking.
  public static void test1() {

    DSForest f = new DSForest();
    try {
      f.add(null);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }

    f.add(s1);

    try {
      f.add(s1);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }

    try {
      f.union(s1, s2);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }

    try {
      f.union(s2, s1);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }
  }

  public static void test2() {

    DSForest f = new DSForest();

    f.add(s1);
    f.add(s2);

    assertPartition(f, s1, null, s2);

    f.union(s1, s2);

    assertPartition(f, s1, s2);

    try {
      f.add(s2);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }

    f.add(s3);

    assertPartition(f, s1, s2, null, s3);

    f.union(s2, s3);

    assertPartition(f, s1, s2, s3);

    f.union(s1, s3);

    assertPartition(f, s1, s2, s3);

    f.union(s1, s2);

    assertPartition(f, s1, s2, s3);
  }

  public static void test3() {

    DSForest f = new DSForest();

    f.add(s1);
    f.add(s2);
    f.add(s3);
    f.add(s4);

    assertPartition(f, s1, null, s2, null, s3, null, s4);

    try {
      f.union(s2, s5);
      fail("Should throw IllegalArgumentException");
    } catch (IllegalArgumentException e) {
      // Good.
    }

    f.union(s1, s2);

    assertPartition(f, s1, s2, null, s3, null, s4);

    f.union(s1, s3);

    assertPartition(f, s1, s2, s3, null, s4);

    f.union(s2, s4);

    assertPartition(f, s1, s2, s3, s4);
  }

  public static void test4() {

    DSForest f = new DSForest();

    f.add(s1);
    f.add(s2);
    f.add(s3);
    f.add(s4);

    f.union(s1, s2);

    f.union(s3, s4);

    assertPartition(f, s1, s2, null, s3, s4);

    f.union(s1, s3);

    assertPartition(f, s1, s2, s3, s4);
  }

  // Checks that the partition f is as specified by ss.
  // The parameter ss specified a partition by enumerating
  // the sets making up the partition. Null elements in ss
  // are used to disntinguish two sets. For example, the partition
  // { { s1, s2 } , { s3} } is represented as the parameters
  // s1, s2, null, s3.
  private static void assertPartition(DSForest f, String... ss) {

    // Create expected partition from ss.
    Set<Set<String>> expectedSets = new LinkedHashSet<Set<String>>();
    Set<String> currentSet = new LinkedHashSet<String>();
    for (String s : ss) {
      if (s == null) {
        expectedSets.add(currentSet);
        currentSet = new LinkedHashSet<String>();
      } else {
        currentSet.add(s);
      }
    }
    expectedSets.add(currentSet);

    Set<Set<String>> actual = f.getSets();

    assert actual.equals(expectedSets);
  }
}
