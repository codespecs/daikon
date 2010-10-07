package MapQuick;

import junit.framework.*;

public class PS4PrivateTest extends TestSuite
{
    public static Test suite() { return new PS4PrivateTest(); }
    public PS4PrivateTest() { this("Problem Set 4 PS4Private Test"); }
    public PS4PrivateTest(String s)
    {
	super(s);
	addTestSuite(StreetNumberSetTest.class);
	addTestSuite(StreetSegmentTest.class);
    }

}

