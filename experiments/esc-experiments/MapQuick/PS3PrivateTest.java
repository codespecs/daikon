package MapQuick;

import junit.framework.*;

public class PrivateTest
{
    static final String TESTDIR_PROP = "test.dir";
    public static Test suite()
    {
	String testDir = System.getProperty(TESTDIR_PROP);
	if (testDir == null)
	    return new TestCase("Loading "+PrivateTest.class.getName()) {
		    protected void runTest() throws Throwable
		    {
			throw new Error("System property not defined: "
					+TESTDIR_PROP);
		    }
		};
	PS3TestSuite.setTestDir(testDir);
	return PS3TestSuite.suite();
    }
}

