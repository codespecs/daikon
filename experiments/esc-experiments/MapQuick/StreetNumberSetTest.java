package MapQuick;

import junit.framework.*;

public class StreetNumberSetTest extends TestCase {

  
  public StreetNumberSetTest(String name) { super(name); }

  public void testEasyContains() {
    StreetNumberSet sns = new StreetNumberSet("3");
    assert("StreetNumberSet '3' doesn't contain 3.", sns.contains(3));
    assert("StreetNumberSet '3' does contain 2.",   !sns.contains(2));
    assert("StreetNumberSet '3' does contain 4.",   !sns.contains(4));
    assert("StreetNumberSet '3' does contain 1.",   !sns.contains(1));
    assert("StreetNumberSet '3' does contain 5.",   !sns.contains(5));
  }

  public void testContainsBoundary() {
    StreetNumberSet sns = new StreetNumberSet("3-9");
    assert("StreetNumberSet '3-9' doesn't contain 3.", sns.contains(3));
    assert("StreetNumberSet '3-9' doesn't contain 9.", sns.contains(9));
    assert("StreetNumberSet '3-9' does contain 1.",   !sns.contains(1));
    assert("StreetNumberSet '3-9' does contain 2.",   !sns.contains(2));
    assert("StreetNumberSet '3-9' does contain 11.",  !sns.contains(11));
    assert("StreetNumberSet '3-9' does contain 10.",  !sns.contains(10));
  }

  public void testContainsInside() {
    StreetNumberSet sns = new StreetNumberSet("3-9");
    assert("StreetNumberSet '3-9' doesn't contain 5.", sns.contains(5));
    assert("StreetNumberSet '3-9' doesn't contain 7.", sns.contains(7));
    assert("StreetNumberSet '3-9' does contain 4.",   !sns.contains(4));
    assert("StreetNumberSet '3-9' does contain 6.",   !sns.contains(6));
    assert("StreetNumberSet '3-9' does contain 8.",   !sns.contains(8));
  }

  public void testContainsRigorous() {
    StreetNumberSet sns = new StreetNumberSet("1-3,7-10001,20221-20223");
    for (int i = 1; i<=3; i+=2) {
      assert("StreetNumberSet '1-3,7-10001,20221-20223' doesn't contain "+i+".",
             sns.contains(i));
    }
    for (int i = 7; i<=10001; i+=2) {
      assert("StreetNumberSet '1-3,7-10001,20221-20223' doesn't contain "+i+".",
             sns.contains(i));
    }
    for (int i = 20221; i<=20223; i+=2) {
      assert("StreetNumberSet '1-3,7-10001,20221-20223' doesn't contain "+i+".",
             sns.contains(i));
    }
  }

  public void testContainsSingleton() {
    StreetNumberSet sns = new StreetNumberSet("1-3,5-505,909,2221-2223");
    assert("StreetNumberSet '1-3,5-505,909,2221-2223' doesn't contain 909.",
           sns.contains(909));
    assert("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 908.",
           !sns.contains(908));
    assert("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 907.",
           !sns.contains(907));
    assert("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 910.",
           !sns.contains(910));
    assert("StreetNumberSet '1-3,5-505,909,2221-2223' does contain 911.",
           !sns.contains(911));
  }

  public void testContainsMixedParity() {
    StreetNumberSet sns = new StreetNumberSet("1-9,6-14");
    assert("StreetNumberSet '1-9,6-14' doesn't contain 3.",
           sns.contains(3));
    assert("StreetNumberSet '1-9,6-14' does contain 4.",
           !sns.contains(4));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 5.",
           sns.contains(5));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 6.",
           sns.contains(6));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 7.",
           sns.contains(7));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 8.",
           sns.contains(8));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 9.",
           sns.contains(9));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 10.",
           sns.contains(10));
    assert("StreetNumberSet '1-9,6-14' does contain 11.",
           !sns.contains(11));
    assert("StreetNumberSet '1-9,6-14' doesn't contain 12.",
           sns.contains(12));
  }

  public void testOrderSimple() {
    StreetNumberSet sns = new StreetNumberSet("1");
    assertEquals("Rank of singleton.", 0, sns.orderStatistic(1));
  }

  public void testOrderMixedParity() {
    StreetNumberSet sns = new StreetNumberSet("1-5,6-14");
    assertEquals("Rank of first element in mixed parity set.",
           0, sns.orderStatistic(1));
    assertEquals("Rank of middle elelemnt in mixed parity set.",
           2, sns.orderStatistic(5));
    assertEquals("Rank of middle elelemnt in mixed parity set.",
           3, sns.orderStatistic(6));
    assertEquals("Rank of last elelemnt in mixed parity set.",
           7, sns.orderStatistic(14));
  }

  public void testOrderMixedParity2() {
    StreetNumberSet sns = new StreetNumberSet("1-11,4-14");
    assertEquals("Rank of first element in mixed parity set.",
           0, sns.orderStatistic(1));
    assertEquals("Rank of middle elelemnt in mixed parity set.",
           3, sns.orderStatistic(5));
    assertEquals("Rank of middle elelemnt in mixed parity set.",
           4, sns.orderStatistic(6));
    assertEquals("Rank of last elelemnt in mixed parity set.",
           11, sns.orderStatistic(14));
  }

  public void testOrderRigorous() {
    StreetNumberSet sns = new StreetNumberSet("10-110,81-2001");
    int j = 0;
    for (int i=10; i<=80; i+=2) {
      assertEquals("Rank in even portion",
             j, sns.orderStatistic(i));
      j++;
    }
    for (int i=81; i<=111; i++) {
      assertEquals("Rank in even/odd portion",
             j, sns.orderStatistic(i));
      j++;
    }
    for (int i=113; i<=2001; i+=2) {
      assertEquals("Rank in odd portion",
             j, sns.orderStatistic(i));
      j++;
    }
  }
    

  
  // Tell JUnit what order to run the tests in
  public static Test suite()
    { 
      TestSuite suite = new TestSuite(); 
      suite.addTest(new StreetNumberSetTest("testEasyContains"));
      suite.addTest(new StreetNumberSetTest("testContainsBoundary"));
      suite.addTest(new StreetNumberSetTest("testContainsInside"));
      suite.addTest(new StreetNumberSetTest("testContainsRigorous"));
      suite.addTest(new StreetNumberSetTest("testContainsSingleton"));
      suite.addTest(new StreetNumberSetTest("testContainsMixedParity"));
      suite.addTest(new StreetNumberSetTest("testOrderSimple"));
      suite.addTest(new StreetNumberSetTest("testOrderMixedParity"));
      suite.addTest(new StreetNumberSetTest("testOrderMixedParity2"));
      suite.addTest(new StreetNumberSetTest("testOrderRigorous"));
      return suite;
    }
  
}
  
  
