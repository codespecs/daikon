package java6170.test;

import junit.framework.*;

/**
   A suite that contains itself; designed to induce an infinite loop in
   unwary callers.
   @see TestTimeout */
public class RecursiveTest extends TestSuite
{
    public RecursiveTest()
    {
	addTest(this);
    }
    public static Test suite()
    {
	return new RecursiveTest();
    }
}
