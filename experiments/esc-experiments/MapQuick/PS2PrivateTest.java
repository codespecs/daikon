package MapQuick;

import junit.framework.*;

public class PrivateTest extends TestSuite
{
    public static Test suite() { return new PrivateTest(); }
    public PrivateTest() { this("Problem Set 2 Private Test"); }
    public PrivateTest(String s)
    {
	super(s);
	addTestSuite(GeoPointTest.class);
	addTestSuite(GeoSegmentTest.class);
	addTestSuite(ElementaryRouteTest.class);
	addTestSuite(CompositeRouteTest.class);
    }

}

