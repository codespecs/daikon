package MapQuick;

import junit.framework.*;
import ps2.GeoPoint;

public class StreetSegmentTest extends TestCase {

  
  public StreetSegmentTest(String name) { super(name); }


  public void testFractionDestBasic() {
    GeoPoint gpDowntown  = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest      = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("1,3,7");
    StreetNumberSet snsl = new StreetNumberSet("");

    StreetSegment ss = new StreetSegment(gpDowntown,
                                         gpWest,
                                         "Western Ave.",
                                         snsr,
                                         snsl,
                                         "02139",
                                         "02138",
                                         StreetClassification.LOCAL_ROAD,
                                         true);
    assertEquals("First address on a sample",
                 0.25,
                 ss.fractionDist(1),
                 0.001);
    assertEquals("Second address on a sample",
                 0.5,
                 ss.fractionDist(3),
                 0.001);
    assertEquals("Third address on a sample",
                 0.75,
                 ss.fractionDist(7),
                 0.001);
  }
    
  public void testFractionDist() {
    GeoPoint gpDowntown  = new GeoPoint(42358333, -71060278);
    GeoPoint gpWest      = new GeoPoint(42358333, -71079857);
    StreetNumberSet snsr = new StreetNumberSet("200-298");
    StreetNumberSet snsl = new StreetNumberSet("201-299");

    StreetSegment ss = new StreetSegment(gpDowntown,
                                         gpWest,
                                         "Western Ave.",
                                         snsr,
                                         snsl,
                                         "02139",
                                         "02138",
                                         StreetClassification.LOCAL_ROAD,
                                         true);
    assertEquals("First address on a long block",
                 0.0196,
                 ss.fractionDist(200),
                 0.001);
    assertEquals("Last address on a long block",
                 0.9804,
                 ss.fractionDist(298),
                 0.001);
    assertEquals("First address on a long block",
                 0.0196,
                 ss.fractionDist(201),
                 0.001);
    assertEquals("Last address on a long block",
                 0.9804,
                 ss.fractionDist(299),
                 0.001);
  }
  
  
  // Tell JUnit what order to run the tests in
  public static Test suite()
    { 
      TestSuite suite = new TestSuite(); 
      suite.addTest(new StreetSegmentTest("testFractionDist"));
      return suite;
    }
  
}
  
