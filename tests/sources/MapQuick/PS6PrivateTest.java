package MapQuick;

import junit.framework.*;

public class PS6PrivateTest extends TestSuite
{
    public static Test suite()
    { return new PS6PrivateTest(); }    
    public PS6PrivateTest()
    { this("Problem Set 6 Private Test"); }  
    public PS6PrivateTest(String s)
    {
	super(s);
	try {
	    addTest(new DirectionsFinderTest());
	} catch (final Throwable t) {
	    addTest(new TestCase("LoadingDirectionsFinder") {
		    protected void runTest() throws Throwable { throw t; }});
	}
	try {
	    addTest(new TextUITest());
	} catch (final Throwable t) {
	    addTest(new TestCase("LoadingTextUI") {
		    protected void runTest() throws Throwable { throw t; }});
	}
    }

  public static void main(String[] args) {
    junit.textui.TestRunner.main(new String[] {
      "MapQuick.PS6PrivateTest",
    });
  }

}
