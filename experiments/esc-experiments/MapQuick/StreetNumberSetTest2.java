package MapQuick;

import junit.framework.*;

public class StreetNumberSetTest2 extends StreetNumberSetTest {
  
  public StreetNumberSetTest2(String name) { super(name); }

  public static void observe(StreetNumberSet s)
  {
    for (int i=0; i<10; i++) {
      int n = (i * 192345) % 1024;
      s.contains(n);
      s.orderStatistic(n);
    }
    s.size();
    s.isEmpty();
    s.min();
    s.max();
    
    //s.intersects();
  }

  public void testVariedNumbers()
  {
    int ticker = 0;
    for (int nrange=1; nrange<=4; nrange++) {
      for (int repeat=1; repeat<=5; repeat++) {
	int i;

	// create some randomish numbers
	int[] starts = new int[nrange];
	int[] ends = new int[nrange];
	for (i=0; i < nrange; i++) {
	  ticker++;
	  starts[i] = Math.abs((ticker * 192345) % 1024);;
	}

	// sort them
	java.util.Arrays.sort(starts);

	// setup end endpoints
	for (i=0; i < nrange-1; i++) {
	  ends[i] = starts[i+1]-10;
	}
	ends[i] = starts[i] + Math.abs(ticker % 200);

	// match parity
	for (i=0; i < nrange; i++) {
	  if ((starts[i] & 1) == 0) {
	    ends[i] &= -2;
	  } else {
	    ends[i] |= 1;
	  }
	}

	// no less than
	for (i=0; i < nrange; i++) {
	  if (ends[i] < starts[i]) {
	    ends[i] = starts[i];
	  }
	}	

	// generate the strings for the ctor
	String fwd = "", bck = "";
	for (i=0; i < nrange; i++) {
	  String rng = starts[i] + "-" + ends[i];
	  fwd = fwd + "," + rng;
	  bck = rng + "," + bck;
	}

	// construct and observe
	observe(sns(fwd.substring(1)));
	if (nrange > 1) {
	  observe(sns(bck.substring(0, bck.length()-1)));
	}

      }
    }
  }

  // Tell JUnit what order to run the tests in
  public static Test suite()
  {
    TestSuite suite = new TestSuite();
    suite.addTest(new TestSuite(StreetNumberSetTest.class));
    suite.addTest(new StreetNumberSetTest2("testVariedNumbers"));
    return suite;
  }

}
