package MapQuick;

import junit.framework.*;

public class PS2PrivateTest extends TestSuite
{
    public static Test suite() { return new PS2PrivateTest(); }
    public PS2PrivateTest() { this("Problem Set 2 Private Test"); }
    public PS2PrivateTest(String s)
    {
	super(s);
	addTestSuite(GeoPointTest.class);
	addTestSuite(GeoSegmentTest.class);
	addTestSuite(ElementaryRouteTest.class);
	addTestSuite(CompositeRouteTest.class);
    }

}

