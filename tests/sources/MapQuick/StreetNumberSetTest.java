package MapQuick;

import MapQuick1.*;
import junit.framework.*;
import org.junit.Test;

public class StreetNumberSetTest extends TestCase {

  public StreetNumberSetTest(String name) {
    super(name);
    // System.err.println(name);
  }

  // convenience function
  static StreetNumberSet sns(String s) {
    return new StreetNumberSet(s);
  }

  @Test
  public void testEasyContains() {
    StreetNumberSet sns = sns("0,3");
    StreetNumberSet sns0 = sns("0");
    assertTrue("StreetNumberSet '0' doesn't contain 0.", sns.contains(0));
    assertTrue("StreetNumberSet '0,3' doesn't contain 3.", sns.contains(3));
    assertTrue("StreetNumberSet '0,3' doesn't contain 3.", sns0.contains(0));
    assertTrue("StreetNumberSet '0,3' does contain 2.", !sns.contains(2));
    assertTrue("StreetNumberSet '0,3' does contain 4.", !sns.contains(4));
    assertTrue("StreetNumberSet '0,3' does contain 1.", !sns.contains(1));
    assertTrue("StreetNumberSet '0,3' does contain 5.", !sns.contains(5));
  }

  @Test
  public void testContainsBoundary() {
    StreetNumberSet sns = sns("3-9");
    assertTrue("StreetNumberSet '3-9' doesn't contain 3.", sns.contains(3));
    assertTrue("StreetNumberSet '3-9' doesn't contain 9.", sns.contains(9));
    assertTrue("StreetNumberSet '3-9' does contain 1.", !sns.contains(1));
    assertTrue("StreetNumberSet '3-9' does contain 2.", !sns.contains(2));
    assertTrue("StreetNumberSet '3-9' does contain 11.", !sns.contains(11));
    assertTrue("StreetNumberSet '3-9' does contain 10.", !sns.contains(10));
  }

  @Test
  public void testContainsInside() {
    StreetNumberSet sns = sns("3-9");
    assertTrue("StreetNumberSet '3-9' doesn't contain 5.", sns.contains(5));
    assertTrue("StreetNumberSet '3-9' doesn't contain 7.", sns.contains(7));
    assertTrue("StreetNumberSet '3-9' does contain 4.", !sns.contains(4));
    assertTrue("StreetNumberSet '3-9' does contain 6.", !sns.contains(6));
    assertTrue("StreetNumberSet '3-9' does contain 8.", !sns.contains(8));
  }

  @Test
  public void testContainsRigorous() {
    StreetNumberSet sns = sns("1-3,7-1001,20221-20223");
    for (int i = 1; i <= 3; i += 2) {
      assertTrue(
          "StreetNumberSet '1-3,7-1001,20221-20223' doesn't contain " + i + ".", sns.contains(i));
    }
    for (int i = 7; i <= 1001; i += 2) {
      assertTrue(
          "StreetNumberSet '1-3,7-1001,20221-20223' doesn't contain " + i + ".", sns.contains(i));
    }
    for (int i = 20221; i <= 20223; i += 2) {
      assertTrue(
          "StreetNumberSet '1-3,7-1001,20221-20223' doesn't contain " + i + ".", sns.contains(i));
    }
  }

  @Test
  public void testContainsSingleton() {
    StreetNumberSet sns = sns("1-3,5-505,909,2221-2223");
    StreetNumberSet sns1_10 = sns("1-3,4-6,7,8,9,10");
    assertTrue("StreetNumberSet '1-3,5-505,909,2221-2223' doesn't contain 909.", sns.contains(909));
    assertTrue("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 908.", !sns.contains(908));
    assertTrue("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 907.", !sns.contains(907));
    assertTrue("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 910.", !sns.contains(910));
    assertTrue("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 911.", !sns.contains(911));
    assertTrue("StreetNumberSet '1-3,4-6,7,8,9,10' doesn't contain 3.", sns1_10.contains(3));
    assertTrue("StreetNumberSet '1-3,4-6,7,8,9,10' does contain 4.", !sns1_10.contains(11));
  }

  @Test
  public void testContainsMixedParity() {
    StreetNumberSet sns = sns("1-9,6-14,19-21,101-109,200-500");
    StreetNumberSet sns2 = sns("1-9,6-14,19-21,101-109,200-500,501-503");
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 3.", sns.contains(3));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' does contain 4.", !sns.contains(4));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 5.", sns.contains(5));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 6.", sns.contains(6));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 7.", sns.contains(7));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 8.", sns.contains(8));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 9.", sns.contains(9));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 10.", sns.contains(10));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' does contain 11.", !sns.contains(11));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' doesn't contain 12.", sns.contains(12));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500' does contain 102.", !sns.contains(102));
    assertTrue(
        "StreetNumberSet '1-9,6-14,19-21,101-109,200-500,501-503' doesn't contain 503.",
        sns2.contains(503));
  }

  @Test
  public void testOrderSimple() {
    StreetNumberSet sns = sns("0");
    assertEquals("Rank of singleton.", 0, sns.orderStatistic(0));
  }

  @Test
  public void testOrderMixedParity() {
    StreetNumberSet sns = sns("1-5,6-14");
    StreetNumberSet sns1 = sns("1,2,3,4,5,6-10");
    StreetNumberSet sns2 = sns("1,2,3,4,6-10");
    assertEquals("Rank of first element in mixed parity set.", 0, sns.orderStatistic(1));
    assertEquals("Rank of middle element in mixed parity set.", 2, sns.orderStatistic(5));
    assertEquals("Rank of middle element in mixed parity set.", 3, sns.orderStatistic(6));
    assertEquals("Rank of last element in mixed parity set.", 7, sns.orderStatistic(14));
    assertEquals("Rank of last element in mixed parity set'", 7, sns1.orderStatistic(10));
    assertEquals("Rank of last element in mixed parity set'", 6, sns2.orderStatistic(10));
  }

  @Test
  public void testOrderMixedParity2() {
    StreetNumberSet sns = sns("1-11,4-14");
    StreetNumberSet sns1 = sns("1-101,4-104,105-109,110-210");
    assertEquals("Rank of first element in mixed parity set.", 0, sns.orderStatistic(1));
    assertEquals("Rank of middle element in mixed parity set.", 3, sns.orderStatistic(5));
    assertEquals("Rank of middle element in mixed parity set.", 4, sns.orderStatistic(6));
    assertEquals("Rank of last element in mixed parity set.", 11, sns.orderStatistic(14));
    assertEquals("Rank of last element in mixed parity set.", 8, sns1.orderStatistic(10));
  }

  @Test
  public void testOrderRigorous() {
    StreetNumberSet sns = sns("10-110,81-2001");
    int j = 0;
    for (int i = 10; i <= 80; i += 2) {
      assertEquals("Rank in even portion", j, sns.orderStatistic(i));
      j++;
    }
    for (int i = 81; i <= 111; i++) {
      assertEquals("Rank in even/odd portion", j, sns.orderStatistic(i));
      j++;
    }
    for (int i = 113; i <= 2001; i += 2) {
      assertEquals("Rank in odd portion", j, sns.orderStatistic(i));
      j++;
    }
  }

  @Test
  public void testEquals1() {
    StreetNumberSet s4_10 = sns("4-10");
    StreetNumberSet s4_10b = sns("4-10");
    StreetNumberSet s4_10c = sns("4-6,8-10");
    StreetNumberSet s4_10d = sns("4-6,8-10");
    assertEquals("'4-10' is not equal to '4-10'", s4_10, s4_10b);
    assertEquals("'4-6,8-10' is not equal to '4-6,8-10'", s4_10c, s4_10d);
  }

  @Test
  public void testEquals2() {
    StreetNumberSet s1_7 = sns("1-7");
    StreetNumberSet s3_517 = sns("3-5,1,7");
    assertEquals("'3-5,1,7' is not equal to '1-7'", s3_517, s1_7);
    assertEquals("'1-7' is not equal to '3-5,1,7'", s1_7, s3_517);
  }

  @Test
  public void testEquals3() {
    StreetNumberSet s1_7 = sns("1-7");
    StreetNumberSet s0 = sns("0");
    StreetNumberSet s3_10 = sns("3,4,5,6,7-9,10");
    assertTrue("'1-7' is equal to '0'", !s1_7.equals(s0));
    assertTrue("'0' is equal to '1-7", !s0.equals(s1_7));
    assertTrue("'1-7' is equal to null", !s1_7.equals(null));
    assertTrue("'1-7' is equal to a String", !s1_7.equals("Foo"));
    assertTrue("'3,4,5,6,7-9,10' equals '0'", !s3_10.equals(s0));
    assertTrue("'0' equals '3,4,5,6,7-9,10'", !s0.equals(s3_10));
    assertTrue("'3,4,5,6,7-9,10' does not equal itself", s3_10.equals(s3_10));
  }

  @Test
  public void testEquals4() {
    StreetNumberSet s3_9 = sns("3-9");
    StreetNumberSet s7_15 = sns("7-15");
    StreetNumberSet s0 = sns("0");
    StreetNumberSet s3_9b = sns("3,4,5,6,7,8,9");
    StreetNumberSet s3_10 = sns("3,4,5,6,7-9,10");
    assertTrue("'3-9' equals '7-15'", !s3_9.equals((Object) s7_15));
    assertTrue("'7-15' does not equal itself", s7_15.equals((Object) s7_15));
    assertTrue("'3,4,5,6,7,8,9' equals '3-9'", !s3_9b.equals((Object) s3_9));
    assertTrue("'7-15' equals '3-9'", !s7_15.equals((Object) s3_9));
    assertTrue("'0' does not equal itself", s0.equals((Object) s0));
    assertTrue("'0' equals '3-9'", !s0.equals((Object) s3_9));
    assertTrue("'3,4,5,6,7,8,9' equals '3,4,5,6,7-9,10'", !s3_9b.equals((Object) s3_10));
    assertTrue("'3,4,5,6,7-9,10' equals '3,4,5,6,7,8,9'", !s3_10.equals((Object) s3_9b));
    assertTrue("'3,4,5,6,7-9,10' equals '3-9'", !s3_9.equals((Object) s3_10));
    assertTrue("'3,4,5,6,7-9,10' equals '0'", !s3_10.equals((Object) s0));
    assertTrue("'3,4,5,6,7,8,9' does not equal itself", s3_9b.equals((Object) s3_9b));
    assertTrue("'3,4,5,6,7-9,10' does not equal itself", s3_10.equals((Object) s3_10));
  }

  @Test
  public void testEquals5() {
    StreetNumberSet s0_9c = sns("5,7-9,0");
    StreetNumberSet s1_9b = sns("1,2,3,4,5,6,7,8,9");
    StreetNumberSet s1_9c = sns("1-3,4-6,7-9");
    StreetNumberSet s1_9d = sns("1,2,3-5,6,7,8,9");
    StreetNumberSet s0_9a = sns("2,4,5,6,7,8,9,0");
    StreetNumberSet s0_9b = sns("2,4,5,6,7,8,9,0");
    StreetNumberSet s1_9e = sns("1-9");
    StreetNumberSet s0 = sns("0");
    StreetNumberSet s1_8a = sns("2,3,1,4,8");
    StreetNumberSet s0_8a = sns("0-2,4,6,8");
    StreetNumberSet s0_8c = sns("0,8");
    StreetNumberSet s0_8b = sns("4,8,6,2,0");
    StreetNumberSet[] set1 = {s0_9c, s1_9b, s1_9c, s1_9d, s0_9a, s1_8a, s0_8a, s0, s1_9e, s0_8c};
    StreetNumberSet[] equalset1 = {s0_9a, s0_9b};
    StreetNumberSet[] equalset2 = {s0_8a, s0_8b};

    for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 10; j++) {
        if (i == j) {
          assertTrue("set is not equal to itself", set1[i].equals(set1[j]));
        } else {
          assertTrue("sets are equal!", !set1[i].equals(set1[j]));
        }
      }
    }

    for (int i = 0; i < 2; i++) {
      for (int j = 0; j < 2; j++) {
        assertTrue("sets are not equal", equalset1[i].equals(equalset1[j]));
        assertTrue("sets are not equal", equalset2[i].equals(equalset2[j]));
        assertTrue("sets are equal!", !equalset1[i].equals(equalset2[j]));
      }
    }
  }

  @Test
  public void testEquals6() {
    StreetNumberSet sns1 = sns("17");
    StreetNumberSet sns2 = sns("1,2,3,4-6");
    StreetNumberSet sns3 = sns("1-19");
    StreetNumberSet sns4 = sns("1-3,6-8,10-12,15-19");
    StreetNumberSet sns5 = sns("0-2,12-20,1-3,13-21,22");
    StreetNumberSet sns6 = sns("1,2,3,4-6");
    assertTrue("'17' is not equal to '17'", sns1.equals(sns1));
    assertTrue("'1,2,3,4-6' is not equal to '1,2,3,4-6'", sns2.equals(sns2));
    assertTrue("'1-19' is not equal to '1-19'", sns3.equals(sns3));
    assertTrue("'1,2,3,4-6' is equal to '1-19'", !sns2.equals(sns3));
    assertTrue("'1-19' is equal to '17'", !sns3.equals(sns2));
    assertTrue("'1-19' is equal to '1-3,6-8,10-12,15-19'", !sns3.equals(sns4));
    assertTrue("'1-3,6-8,10-12,15-19' is equal to '1-19'", !sns4.equals(sns3));
    assertTrue("'1-3,6-8,10-12,15-19' is not equal to itself", sns4.equals(sns4));
    assertTrue("'1-3,6-8,10-12,15-19' is equal to '1,2,3,4-6'", !sns2.equals(sns4));
    assertTrue("'1,2,3,4-6' is equal to '1-3,6-8,10-12,15-19'", !sns4.equals(sns2));
    assertTrue("'0-2,12-20,1-3,13-21,22' does not equal itself", sns5.equals(sns5));
    assertTrue("'1,2,3,4-6' does not equal '1,2,3,4-6'", sns2.equals(sns6));
    assertTrue("'1,2,3,4-6' does not equal '1,2,3,4-6'", sns6.equals(sns2));
    assertTrue("'0-2,12-20,1-3,13-21,22' does not equal '1,2,3,4-6'", !sns5.equals(sns6));
  }

  @Test
  public void testHashCode() {
    StreetNumberSet s1 = sns("1-7");
    StreetNumberSet s2 = sns("3-5,1,7");
    assertEquals(
        "hashCode for '1-7' is not the same as hashCode for '3-5,1,7'",
        s1.hashCode(),
        s2.hashCode());
  }

  @Test
  public void testSize() {
    StreetNumberSet s0 = sns("0");
    StreetNumberSet s2_8 = sns("2,3,4,5,6,7,8");
    assertEquals("size of '0' is not equal to 1", s0.size(), 1);
    assertEquals("size of '2,3,4,5,6,7,8' is not equal to 4", s2_8.size(), 7);
  }

  @Test
  public void testIsEmpty() {
    StreetNumberSet s0 = sns("3,4,5");
    StreetNumberSet s1 = sns("0,1,2,3,4,5,6");
    assertTrue("'3,4,5' is empty", !s0.isEmpty());
    assertTrue("'0,1,2,3,4,5,6' is empty", !s1.isEmpty());
  }

  @Test
  public void testIntersects() {
    StreetNumberSet[] intersect = {
      sns("4-10"),
      sns("2-10,12-18,0"),
      sns("0,4,6,8,10,12,16,18"),
      sns("6"),
      sns("4,6-18"),
      sns("0,2,4,6,8"),
      sns("0-20")
    };

    StreetNumberSet s13_16 = sns("22,23-25,27");
    for (int i = 0; i < 7; i++) {
      assertTrue("sets intersects", !intersect[i].intersects(s13_16));
      assertTrue("sets intersects", !s13_16.intersects(intersect[i]));
      for (int j = 0; j < 7; j++) {
        assertTrue("sets do not intersect", intersect[i].intersects(intersect[j]));
      }
    }
  }

  // Tell JUnit what order to run the tests in
  public static Test suite() {
    // includes all the tests from this file (in arbitrary order)
    TestSuite suite = new TestSuite(StreetNumberSetTest.class);
    return suite;
  }
}
