package java6170.test;

import junit.framework.*;
import java.lang.reflect.*;
import java.io.*;

public class FilterTest extends TestCase
{
    private static final int INITIAL_BUFFER_SIZE = 10240; // 10 kilobytes
    private static final boolean CAPTURE_STDERR = false;

    private BufferedReader _expected = null;
    private InputStream _testInput = null;
    private Method _mainMethod = null;
    /**
       @param mainClass class that defines a main() that reads from
                        System.in and writes to System.out
       @param testInput test input for the filter
       @param expected  expected test output
     */
    public FilterTest(String description,
		      Class mainClass,
		      InputStream testInput,
		      Reader expected)
    {
	super(description);
	assertNotNull(mainClass);
	assertNotNull(testInput);
	assertNotNull(expected);
	
	_testInput = testInput;
	_expected = new BufferedReader(expected);
	
	// grab the "main" method from mainClass
	try {
	    _mainMethod = mainClass.getMethod
		("main", new Class[] { String[].class } );
	} catch (NoSuchMethodException e) {
	    throw new IllegalArgumentException
		(mainClass.getName()+" does not define main()");
	} 
	if (!Modifier.isPublic(_mainMethod.getModifiers())
	    || !Modifier.isStatic(_mainMethod.getModifiers()))
	    throw new IllegalArgumentException
		(mainClass.getName()+" main() is not public and static");
	    
    }

    /**
       Runs the main() method in this.mainClass, piping this.testInput into
       its standard input.  Standard output is captured and compared to
       this.expected line-by-line.
       @throws Throwable exception or error from running mainClass
    */
    protected void runTest() throws Throwable
    {
	// save the old streams
	PrintStream savedSystemOut = System.out;
	PrintStream savedSystemErr = System.err;
	InputStream savedSystemIn  = System.in;

	// read input from testInput and capture output in buffers
	ByteArrayOutputStream outBuffer =
	    new ByteArrayOutputStream(INITIAL_BUFFER_SIZE);
	ByteArrayOutputStream errBuffer =
	    new ByteArrayOutputStream(INITIAL_BUFFER_SIZE);
	PrintStream testOutput =
	    new PrintStream(outBuffer, true); // autoflush
	PrintStream testError =
	    new PrintStream(outBuffer, true); // autoflush
	System.setIn(_testInput);
	System.setOut(testOutput);
	if (CAPTURE_STDERR) {
	    System.setErr(testError);
	}

	// invoke the method
	try {
	    _mainMethod.invoke(null, mainParams());
	} catch (InvocationTargetException e) {
	    throw e.getTargetException();
	}

	// flush streams
	System.out.flush();
	System.err.flush();
	
	// compare actual output to expected
	BufferedReader actual = new BufferedReader
	    (new StringReader(outBuffer.toString()));
	String actualLine = null;
	String expectedLine = null;
	int lineNumber = 1;
	do {
	    actualLine = actual.readLine();
	    expectedLine = _expected.readLine();
	    assertLineMatch(lineNumber, expectedLine, actualLine);
	    lineNumber++;
	} while (expectedLine != null);
	
	// compare actual error to expected
	// XXX IGNORED
	
	// restore the old streams
	System.setIn(savedSystemIn);
	System.setOut(savedSystemOut);
	if (CAPTURE_STDERR) {
	    System.setErr(savedSystemErr);
	}
    }

    protected Object[] mainParams()
    {
	return new Object[] { new String[0] };
    }

    /**
       Compares expected and actual output lines for end-of-file status.
       @param line_no number of the line being compared
       @param expected expected line; null if end-of-file
       @param actual actual line; null if end-of-file
       @throws AssertionFailedError if there's a mismatch */
    protected void assertEofMatch(int lineNum, String expected, String actual)
    {
	// check whether one is null and the other is not
	if ((expected == null) == (actual == null)) return;

	expected = (expected == null)?"end of file":("<"+expected+">");
	actual = (actual == null)?"end of file":("<"+actual+">");
	fail("mismatch on line "+lineNum
	     +": expected "+expected
	     +", but was "+actual);
    }
    
    /**
       Compares expected and actual output lines.  Asserts that lines are
       equal and reports the line number, expected, and actual values
       otherwise. Subclasses should override to get specific behavior or
       error messages.
       @param line_no number of the line being compared
       @param expected expected line; null if end-of-file
       @param actual actual line; null if end-of-file
       @throws AssertionFailedError if there's a mismatch */
    protected void assertLineMatch(int lineNum, String expected, String actual)
    {
	assertEofMatch(lineNum, expected, actual);
	assertEquals("mismatch on line "+lineNum+":", expected, actual);
    }

}
