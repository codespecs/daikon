package MapQuick;

import MapQuick1.*;
import junit.framework.*;

public class StreetNumberSetTest2 extends StreetNumberSetTest {

  public static void main(String[] args) {
    // call everything directly so that Ajax can understand
    run_directly();
  }

  // Toggles behavior between original version and PAG-augmented version
  private static final boolean use_original = false;

  public static void run_directly() {
    if (!use_original) {
      StreetNumberSetTest2 t2;
      t2 = new StreetNumberSetTest2("testVariedNumbers");
      t2.testVariedNumbers();
      t2 = new StreetNumberSetTest2("testSpecial");
      t2.testSpecial();
    }
    StreetNumberSetTest t;
    t = new StreetNumberSetTest("testEasyContains");
    t.testEasyContains();
    t = new StreetNumberSetTest("testContainsBoundary");
    t.testContainsBoundary();
    t = new StreetNumberSetTest("testContainsInside");
    t.testContainsInside();
    t = new StreetNumberSetTest("testContainsRigorous");
    t.testContainsRigorous();
    t = new StreetNumberSetTest("testContainsSingleton");
    t.testContainsSingleton();
    t = new StreetNumberSetTest("testContainsMixedParity");
    t.testContainsMixedParity();
    t = new StreetNumberSetTest("testOrderSimple");
    t.testOrderSimple();
    t = new StreetNumberSetTest("testOrderMixedParity");
    t.testOrderMixedParity();
    t = new StreetNumberSetTest("testOrderMixedParity2");
    t.testOrderMixedParity2();
    t = new StreetNumberSetTest("testOrderRigorous");
    t.testOrderRigorous();
    t = new StreetNumberSetTest("testEquals1");
    t.testEquals1();
    t = new StreetNumberSetTest("testEquals2");
    t.testEquals2();
    t = new StreetNumberSetTest("testEquals3");
    t.testEquals3();
    t = new StreetNumberSetTest("testEquals4");
    t.testEquals4();
    t = new StreetNumberSetTest("testEquals5");
    t.testEquals5();
    t = new StreetNumberSetTest("testEquals6");
    t.testEquals6();
    t = new StreetNumberSetTest("testHashCode");
    t.testHashCode();
    t = new StreetNumberSetTest("testSize");
    t.testSize();
    t = new StreetNumberSetTest("testIsEmpty");
    t.testIsEmpty();
    t = new StreetNumberSetTest("testIntersects");
    t.testIntersects();
  }

  public StreetNumberSetTest2(String name) {
    super(name);
  }

  public static void observe(StreetNumberSet s) {
    for (int i = 0; i < 10; i++) {
      int n = (i * 192345) % 1024;
      s.contains(n);
      s.orderStatistic(n);
    }
    s.size();
    s.isEmpty();
    s.min();
    s.max();
    s.equals((Object) null);
    s.equals((StreetNumberSet) null);
    s.equals((Object) s);
    s.equals(s);
    s.equals(new StreetNumberSet("1"));
    s.equals(new StreetNumberSet("1,7"));
    s.equals(new StreetNumberSet("1,7,11"));

    s.intersects(s);
    s.intersects(sns("1,100,10"));
  }

  @Test
  public void testSpecial() {
    observe(sns("3,2,1"));
  }

  @Test
  public void testVariedNumbers() {
    int ticker = 0;
    for (int nrange = 1; nrange <= 4; nrange++) {
      for (int repeat = 1; repeat <= 5; repeat++) {
        int i;

        // create some randomish numbers
        int[] starts = new int[nrange];
        int[] ends = new int[nrange];
        for (i = 0; i < nrange; i++) {
          ticker++;
          starts[i] = Math.abs((ticker * 192345) % 1024);
          ;
        }

        // sort them
        java.util.Arrays.sort(starts);

        // setup end endpoints
        for (i = 0; i < nrange - 1; i++) {
          ends[i] = starts[i + 1] - 10;
        }
        ends[i] = starts[i] + Math.abs(ticker % 200);

        // match parity
        for (i = 0; i < nrange; i++) {
          if ((starts[i] & 1) == 0) {
            ends[i] &= -2;
          } else {
            ends[i] |= 1;
          }
        }

        // no less than
        for (i = 0; i < nrange; i++) {
          if (ends[i] < starts[i]) {
            ends[i] = starts[i];
          }
        }

        // generate the strings for the ctor
        String fwd = "", bck = "";
        for (i = 0; i < nrange; i++) {
          String rng = starts[i] + "-" + ends[i];
          fwd = fwd + "," + rng;
          bck = rng + "," + bck;
        }

        // construct and observe
        observe(sns(fwd.substring(1)));
        if (nrange > 1) {
          observe(sns(bck.substring(0, bck.length() - 1)));
        }
      }
    }
  }

  // Tell JUnit what order to run the tests in
  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTest(new TestSuite(StreetNumberSetTest.class));
    suite.addTest(new StreetNumberSetTest2("testVariedNumbers"));
    return suite;
  }
}
