package MapQuick;

import MapQuick2.*;


import junit.framework.*;

// TODO: check to see what happens if you have a path that loops back
// upon itself; either immediately or if it ever reaches the same
// GeoPoint twice.

public class ElementaryRouteTest extends TestCase {

  private static final double tolerance = GeoPointTest.tolerance;

  private GeoPoint gpDowntown = null;
  private GeoPoint gpWest     = null;
  private GeoPoint gpWest2    = null;
  private GeoPoint gpNorthWest2    = null;


  private GeoSegment gsWest  = null;
  private GeoSegment gsWest2 = null;
  private GeoSegment gsNorthWest2 = null;

  private ElementaryRoute rWest = null;
  private ElementaryRoute rWest2 = null;

  public ElementaryRouteTest(String name) { super(name); }

  protected void setUp() {
    // +1 mile                   14488      19579
    gpDowntown   = new GeoPoint(42358333, -71060278);
    gpWest       = new GeoPoint(42358333, -71079857);
    gpWest2      = new GeoPoint(42358333, -71099436);
    gpNorthWest2 = new GeoPoint(42372821, -71099436);

    gsWest = new GeoSegment("West",  gpDowntown, gpWest);
    gsWest2 = new GeoSegment("West", gpWest, gpWest2);
    gsNorthWest2 = new GeoSegment("West", gpWest2,gpNorthWest2);

    rWest = new ElementaryRoute(gsWest);
    rWest2 = new ElementaryRoute(gsWest2);
  }


  private static String stripWhiteSpace(String s) {
    char[] inBuff = s.toCharArray();
    char[] outBuff = new char[inBuff.length];
    int l = 0;

    for (int i=0; i<inBuff.length; i++) {
      if (!Character.isWhitespace(inBuff[i])) {
        outBuff[l] = inBuff[i];
        l++;
      }
    }
    return new String(outBuff, 0, l);
  }

  private static String removeString(String source, String pattern) {
    int t = source.indexOf(pattern);
    if (t < 0) return source;
    String firstHalf = source.substring(0,t);
    String secondHalf = source.substring(t+2);
    return firstHalf+secondHalf;
  }

  private static boolean almostEquals(String a, String b) {
    a = stripWhiteSpace(a);
    b = stripWhiteSpace(b);
    a = removeString(a,"to");
    b = removeString(b,"to");
    a = a.toLowerCase();
    b = b.toLowerCase();
    return a.equals(b);
  }

  private static void assertAlmostEquals(String message, String a, String b) {
    if (!almostEquals(a,b)) fail(message);
  }

  public void testImmutability() {
    ElementaryRoute rReceiver =
      new ElementaryRoute(new GeoSegment("West",  gpDowntown, gpWest));
    ElementaryRoute rArgument =
      new ElementaryRoute(new GeoSegment("West",  gpWest, gpWest2));
    GeoSegment gsArgument = new GeoSegment("West",  gpWest, gpWest2);
    rReceiver.addSegment(gsArgument);
    assertTrue("addSegment mutates receiver.", rReceiver.equals(rWest));
    assertTrue("addSegment mutates argument.", gsArgument.equals(gsWest2));
    rReceiver.directions(0.0);
    assertTrue("directions mutates receiver.", rReceiver.equals(rWest));
    rReceiver.hashCode();
    assertTrue("hashCode mutates receiver.", rReceiver.equals(rWest));
    rReceiver.name();
    assertTrue("name mutates receiver.", rReceiver.equals(rWest));
    rReceiver.elementaryRoutes();
    assertTrue("elementaryRoutes mutates receiver.", rReceiver.equals(rWest));
    rReceiver.end();
    assertTrue("end mutates receiver.", rReceiver.equals(rWest));
    rReceiver.endHeading();
    assertTrue("endHeading mutates receiver.", rReceiver.equals(rWest));
    rReceiver.length();
    assertTrue("length mutates receiver.", rReceiver.equals(rWest));
    rReceiver.start();
    assertTrue("start mutates receiver.", rReceiver.equals(rWest));
    rReceiver.startHeading();
    assertTrue("startHeading mutates receiver.", rReceiver.equals(rWest));
  }


  public void testName(){
    assertTrue("the name rWest is not the same than the name of the segment gsWest it was created with",rWest.name().equals(gsWest.name()));
  }

  public void  testStart(){
    assertTrue("the start point of rWest is not the same than the start point of the segment gsWest it was created with",rWest.start().equals(gpDowntown));
  }

  public void testEnd(){
    assertTrue("the end point of rWest is not the same than the start point of the segment gsWest it was created with",rWest.end().equals(gpWest));
  }

  public void testStartHeading(){
      assertEquals("the start heading of rWest is not the same the heading of the segment gWest it was created with",rWest.startHeading(),gsWest.heading(),tolerance);
  }

  public void testEndHeading(){
      assertEquals("the end heading of rWest is not the same the heading of the segment gWest it was created with",rWest.endHeading(),gsWest.heading(),tolerance);

  }

  public void testLength(){
      assertEquals("the length of rWest is not the same the length of the segment gWest it was created with",rWest.length(),gsWest.length(),tolerance);
  }

  public void testEquals(){
    ElementaryRoute rWestbis = new ElementaryRoute(gsWest);
    assertTrue("rWest and rWestBis are created both from gsWest, but they are not equal",rWest.equals(rWestbis));
  }

  public void testAddSegment(){
    ElementaryRoute r = rWest.addSegment(gsWest2);

    // test the spec
    assertEquals("the length of rWest.addSegment(gsWest2) is not the sum of the length of rWest + gsWest2",r.length(), (rWest.length() + gsWest2.length()), tolerance);
    assertTrue("the end point rWest.addSegment(gsWest2) is not the same end point than gsWest2",r.end().equals(gpWest2));
    assertTrue("the start point rWest.addSegment(gsWest2) is not the same start point than rWest",r.start().equals(gpDowntown));
    assertEquals("the end heading of rWest.addSegment(gsWest2) is not the same the heading of gsWest2",r.endHeading(),gsWest2.heading(), tolerance);
    assertEquals("the start heading of rWest.addSegment(gsWest2) is not the same the start heading of rWest",r.startHeading(),gsWest.heading(),tolerance);

    // test to see that the original ER wasn't mutated
    // might not pass if their equals is broken
    ElementaryRoute rWestbis = new ElementaryRoute(gsWest);
    assertTrue("after rWest.addSegment(gsWest2), rWest was mutated!!",rWest.equals(rWestbis));
  }


  public void testEquals2(){
    GeoSegment gsWestNorthWest2 = new GeoSegment("West",gpWest,gpNorthWest2);
    GeoSegment gsWest2NorthWest2 = new GeoSegment("West",gpWest2,gpNorthWest2);
    ElementaryRoute rDW2 = new ElementaryRoute(gsWestNorthWest2);
    ElementaryRoute rDWW2 = rWest2.addSegment(gsWest2NorthWest2);

    // this test relies on a correct addSegment()
    assertTrue("r1 is created from (gpWest -> gpWest2) + (gpWest2 -> gpNorthWest2), it should not be equal to r2 created from (gpWest -> gpNorthWest2)",!rDW2.equals(rDWW2));
  }


  public void testElementaryRoutes(){
    ElementaryRoute[] era = rWest.elementaryRoutes();
    if(era == null)
      fail("the array of ElementaryRoute is null, it should contain exactly 1 element");
    else if(era.length == 0)
      fail("the array of ElementaryRoute is empty, it should contain exactly one element");
    else if(era.length != 1)
      fail("the array of ElementaryRoute has a size bigger than 1, it should contain exactly one element");

    else assertTrue("the array of ElementaryRoutes of the ElementaryRoute r doesn't contain r",era[0].equals(rWest));
  }

  public void testDirections(){

    ElementaryRoute r0 = rWest.addSegment(gsWest2);
    ElementaryRoute r1 = r0.addSegment(gsNorthWest2);

    assertEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(270)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(270));
    assertEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(279)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(279));
    assertEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(261)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(261));

    assertEquals("should be: 'Turn slight left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(280)+"'","Turn slight left onto West and go 3.0 miles.\n", r1.directions(280));
    assertEquals("should be: 'Turn slight left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(329)+"'","Turn slight left onto West and go 3.0 miles.\n", r1.directions(329));

    assertEquals("should be: 'Turn slight right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(260)+"'","Turn slight right onto West and go 3.0 miles.\n", r1.directions(260));
    assertEquals("should be: 'Turn slight right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(211)+"'","Turn slight right onto West and go 3.0 miles.\n", r1.directions(211));

    assertEquals("should be: 'Turn left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(330)+"'","Turn left onto West and go 3.0 miles.\n", r1.directions(330));
    assertEquals("should be: 'Turn left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(29)+"'","Turn left onto West and go 3.0 miles.\n", r1.directions(29));

    assertEquals("should be: 'Turn right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(210)+"'","Turn right onto West and go 3.0 miles.\n", r1.directions(210));
    assertEquals("should be: 'Turn right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(151)+"'","Turn right onto West and go 3.0 miles.\n", r1.directions(151));

    assertEquals("should be: 'Turn sharp left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(30)+"'","Turn sharp left onto West and go 3.0 miles.\n", r1.directions(30));
    assertEquals("should be: 'Turn sharp left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(88)+"'","Turn sharp left onto West and go 3.0 miles.\n", r1.directions(88));

    assertEquals("should be: 'Turn sharp right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(150)+"'","Turn sharp right onto West and go 3.0 miles.\n", r1.directions(150));
    assertEquals("should be: 'Turn sharp right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(92)+"'","Turn sharp right onto West and go 3.0 miles.\n", r1.directions(92));

    assertEquals("should be: 'U-turn onto West and go 3.0 miles.\n' but it is: '"+r1.directions(90)+"'","U-turn onto West and go 3.0 miles.\n", r1.directions(90));
  }


  public void testApproxDirections(){

    ElementaryRoute r0 = rWest.addSegment(gsWest2);
    ElementaryRoute r1 = r0.addSegment(gsNorthWest2);

    assertAlmostEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(270)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(270));
    assertAlmostEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(279)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(279));
    assertAlmostEquals("should be: 'Continue onto West and go 3.0 miles.\n' but it is: '"+r1.directions(261)+"'","Continue onto West and go 3.0 miles.\n", r1.directions(261));

    assertAlmostEquals("should be: 'Turn slight left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(280)+"'","Turn slight left onto West and go 3.0 miles.\n", r1.directions(280));
    assertAlmostEquals("should be: 'Turn slight left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(329)+"'","Turn slight left onto West and go 3.0 miles.\n", r1.directions(329));

    assertAlmostEquals("should be: 'Turn slight right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(260)+"'","Turn slight right onto West and go 3.0 miles.\n", r1.directions(260));
    assertAlmostEquals("should be: 'Turn slight right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(211)+"'","Turn slight right onto West and go 3.0 miles.\n", r1.directions(211));

    assertAlmostEquals("should be: 'Turn left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(330)+"'","Turn left onto West and go 3.0 miles.\n", r1.directions(330));
    assertAlmostEquals("should be: 'Turn left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(29)+"'","Turn left onto West and go 3.0 miles.\n", r1.directions(29));

    assertAlmostEquals("should be: 'Turn right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(210)+"'","Turn right onto West and go 3.0 miles.\n", r1.directions(210));
    assertAlmostEquals("should be: 'Turn right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(151)+"'","Turn right onto West and go 3.0 miles.\n", r1.directions(151));

    assertAlmostEquals("should be: 'Turn sharp left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(30)+"'","Turn sharp left onto West and go 3.0 miles.\n", r1.directions(30));
    assertAlmostEquals("should be: 'Turn sharp left onto West and go 3.0 miles.\n' but it is: '"+r1.directions(88)+"'","Turn sharp left onto West and go 3.0 miles.\n", r1.directions(88));

    assertAlmostEquals("should be: 'Turn sharp right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(150)+"'","Turn sharp right onto West and go 3.0 miles.\n", r1.directions(150));
    assertAlmostEquals("should be: 'Turn sharp right onto West and go 3.0 miles.\n' but it is: '"+r1.directions(92)+"'","Turn sharp right onto West and go 3.0 miles.\n", r1.directions(92));

    assertAlmostEquals("should be: 'U-turn onto West and go 3.0 miles.\n' but it is: '"+r1.directions(90)+"'","U-turn onto West and go 3.0 miles.\n", r1.directions(90));
  }


    // Tell JUnit what order to run the tests in
  public static Test suite()
  {
    TestSuite suite = new TestSuite();
    suite.addTest(new ElementaryRouteTest("testName"));
    suite.addTest(new ElementaryRouteTest("testStart"));
    suite.addTest(new ElementaryRouteTest("testEnd"));
    suite.addTest(new ElementaryRouteTest("testStartHeading"));
    suite.addTest(new ElementaryRouteTest("testEndHeading"));
    suite.addTest(new ElementaryRouteTest("testLength"));
    suite.addTest(new ElementaryRouteTest("testEquals"));
    suite.addTest(new ElementaryRouteTest("testAddSegment"));
    suite.addTest(new ElementaryRouteTest("testEquals2"));
    suite.addTest(new ElementaryRouteTest("testElementaryRoutes"));
    suite.addTest(new ElementaryRouteTest("testDirections"));
    suite.addTest(new ElementaryRouteTest("testApproxDirections"));
    suite.addTest(new ElementaryRouteTest("testImmutability"));

    return suite;
  }


}
