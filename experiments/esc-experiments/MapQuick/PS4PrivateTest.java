package MapQuick;

import junit.framework.*;

public class PrivateTest extends TestSuite
{
    public static Test suite() { return new PrivateTest(); }
    public PrivateTest() { this("Problem Set 4 Private Test"); }
    public PrivateTest(String s)
    {
	super(s);
	addTestSuite(StreetNumberSetTest.class);
	addTestSuite(StreetSegmentTest.class);
    }

}

