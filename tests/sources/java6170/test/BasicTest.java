package java6170.test;

import junit.framework.*;

public class BasicTest extends TestCase
{
    public BasicTest(String name)
    {
	super(name);
    }
    public void testSuccess()
    {
	System.out.println("This test should pass");
    }
    public void testFailure()
    {
	assertTrue("This test should fail", false);
    }
    public void testTimeout()
    {
	while (true);
    }
    public void testExit()
    {
	System.exit(1);
    }
    public void testError()
    {
	throw new Error("Doh!");
    }
    public void testException() throws Exception
    {
	throw new Exception("I take exception to that!");
    }
}
