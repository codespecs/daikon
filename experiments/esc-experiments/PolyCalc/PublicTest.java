package PolyCalc;

import junit.framework.*;

public class PublicTest extends TestSuite
{
    public static Test suite() { return new PublicTest(); }
    public PublicTest() { this("Problem Set 1 Public Test"); }
    public PublicTest(String s)
    {
	super(s);
	addTestSuite(RatNumTest.class);
	addTestSuite(RatPolyTest.class);
	addTestSuite(RatPolyStackTest.class);
    }

}
