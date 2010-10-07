package java6170.test;

import junit.framework.*;

/**
   A Test whose constructor enters an infinite loop, designed to trap unwary
   callers.  */
public class InfiniteTest extends TestSuite
{
    public InfiniteTest()
    {
	while (true);
    }
    public static Test suite()
    {
	return new InfiniteTest();
    }
}
