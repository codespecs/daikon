package java6170.test;

import junit.framework.*;
import java.util.*;
import java.io.*;

/**
   TestSuite that creates a Test for each test description (a String) read
   from a Reader.  Default implementation reads lines from a file.  */
public class TestReader extends TestSuite
{
    private BufferedReader _reader;
    private List _strings;
    /**
       Constructs a TestSuite from the given Reader: Breaks input into
       Strings using readStrings() and creates a test for each String using
       createTest().  Subclasses should override those methods to customize.
    */
    public TestReader(Reader reader)
    {
	super();
	_reader = new BufferedReader(reader);
	_strings = readStrings();
	checkStrings();
	Iterator strings = _strings.iterator();
	while (strings.hasNext()) {
	    Test test = createTest((String)strings.next());
	    if (test == null) continue;
	    addTest(test);
	}
    }
    /** Constructs from the given file.  */
    public TestReader(String filename) throws FileNotFoundException
    {
	this(new FileReader(filename));
    }
    /**
       Checks that _strings list is non-null and
       contains only Strings.
       @throws IllegalStateException if check fails
     */
    private void checkStrings()
    {
	if (_strings == null)
	    throw new IllegalStateException
		("strings must not be null");
	Iterator strings = _strings.iterator();
	while (strings.hasNext())
	    if (!(strings.next() instanceof String))
	    throw new IllegalStateException
		("strings must contain only Strings");
    }
    /**
       Access Reader.
     */
    protected BufferedReader getReader()
    {
	return _reader;
    }
    /**
       Reads from Reader and returns a list of Strings from which to create
       tests.  Subclasses can override to read different strings.
    */
    protected List readStrings()
    {
	List strings = new Vector();
	try {
	    while (true) {
		String string = getReader().readLine();
		if (string == null) break; // EOF
		strings.add(string);
	    }
	} catch (IOException e) {
	    // ignored
	}
	return strings;
    }
    /**
       Creates a Test from a test description (a String).  Default
       implementation returns null (which is ignored).  */
    protected Test createTest(String string)
    {
	return null;
    }
}
