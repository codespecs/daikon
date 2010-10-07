package java6170.test;

import junit.framework.*;
import java.io.*;

/**
   For testing DirectorySuite and FilterTest.
   Simple filter that reads lines from stdin and writes to stdout.
   Expects .test and .expected files in calling directory (".").
 */
public class DirTest
{
    public static void main(String[] args)
	throws IOException
    {
	BufferedReader in = new BufferedReader
	    (new InputStreamReader(System.in));
	while (true) {
	    String line = in.readLine();
	    if (line == null) break;
	    System.out.println(line);
	}
    }
    public static Test suite()
    {
	try {
	    return new DirectorySuite(".", DirTest.class);
	} catch (final Throwable t) {
	    return new TestCase("Loading DirTest") {
		    protected void runTest() throws Throwable
		    {
			throw t;
		    }
		};
	}
    }
}
