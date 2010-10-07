package java6170.test;

import junit.framework.*;
import junit.extensions.*;
import java.util.*;
import java.io.*;

public class Grader implements TestRunListener
{
    private PrintStream _out = System.out;
    private Map _classToTest = new LinkedHashMap(); // class -> test -> points
    private Map _classToPoints = new LinkedHashMap(); // class -> <points, total>
    private boolean _gotFail = false;
    public void init(String[] args)
    {
	for (int i = 0; i < args.length; i++) {
	    if (args[i].equals("--help"))
		printUsage();
	    else if (args[i].startsWith("-"))
		printUsage("Unrecognized option: "+args[i]);
	    else {
		try {
		    loadPoints(new FileReader(args[i]));
		} catch (IOException e) {
		    printUsage(e.toString());
		}
	    }
	}
    }
    private void loadPoints(Reader in) throws IOException
    {
	BufferedReader reader = new BufferedReader(in);
	while (true) {
	    String line = reader.readLine();
	    if (line == null) break;
	    loadLine(line);
	}
    }
    private void loadLine(String line)
    {
	if (line.startsWith("#"))
	    return; // skip comments
	StringTokenizer tokens = new StringTokenizer(line);
	if (tokens.countTokens() == 0)
	    return; // skip blanks
	if (tokens.countTokens() != 3)
	    printLineUsage(line);
	String classname = tokens.nextToken();
	String testname = tokens.nextToken();
	float points = 0;
	try {
	    points = Float.parseFloat(tokens.nextToken());
	} catch (NumberFormatException e) {
	    printLineUsage(line);
	}
	if (points < 0)
	    printLineUsage(line);
	if (testname.equals("base")) {
	    ClassPoints cp = new ClassPoints();
	    cp.points = points;
	    cp.total  = points;
	    setClassPoints(classname, cp);
	} else {
	    setTestPoints(classname, testname, points);
	}
    }
    /**
       Set the points for test testname in class classname to points.
     */
    private void setTestPoints(String classname, String testname,
			       float points)
    {
	Map testToPoints = (Map)_classToTest.get(classname);
	if (testToPoints == null) {
	    testToPoints = new LinkedHashMap();
	    _classToTest.put(classname, testToPoints);
	}
	testToPoints.put(testname, new Float(points));
    }
    /**
       Get the points for test testname in class classname.
       If no specific value set for that test, returns default for that class.
       If no default for that class, return null.
     */
    private Float getTestPoints(String classname, String testname)
    {
	Map testToPoints = (Map)_classToTest.get(classname);
	if (testToPoints == null)
	    return null;
	Float points = (Float)testToPoints.get(testname);
	if (points == null)
	    return (Float)testToPoints.get("default");
	return points;
    }
    private void printLineUsage(String line)
    {
	throw new IllegalArgumentException("Grader: line format is:\n<classname> (<testname>|default|base) <points: positive float>\nbad line: "+line);
    }
    private void printUsage()
    {
	printUsage(null);
    }
    private void printUsage(String why)
    {
	StringBuffer buf = new StringBuffer();
	if (why != null)
	    buf.append(why).append('\n');
	buf.append("Usage: Grader [options] <gradeFile>*\n");
	buf.append("--help: print this help message\n");
	buf.append("gradeFile: each line gives point value for a test:\n");
	buf.append("# comment line\n");
	buf.append("<classname> <testname> <points>\n");
	buf.append("<classname> default <points>\n");
	buf.append("<classname> base <points>\n");
	buf.append("default: default points for tests in that class\n");
	buf.append("base: starting number of points for that class\n");
	buf.append("points: positive float\n");
	throw new IllegalArgumentException(buf.toString());
    }
    public void startRun(TestResult result)
    {
    }
    public void startTest(Test test)
    {
	_gotFail = false;
    }
    public void addFailure(Test test, AssertionFailedError t)
    {
	_gotFail = true;
    }
    public void addError(Test test, Throwable t)
    {
	_gotFail = true;
    }
    public void endTest(Test test)
    {
	if (!(test instanceof TestCase)) return;
	TestCase tcase = (TestCase)test;
	String classname = tcase.getClass().getName();
	String testname = tcase.name();
	Float points = getTestPoints(classname, testname);
	if (points == null)
	    throw new IllegalStateException
		("Grader: unrecognized test: "+classname+" "+testname);
	ClassPoints cp = getClassPoints(classname);
	cp.total += points.floatValue();
	if (!_gotFail)
	    cp.points += points.floatValue();
	setClassPoints(classname, cp);
    }
    public void endRun(TestResult result)
    {
	_out.println("GRADES");
	Iterator classnames = _classToPoints.keySet().iterator();
	while (classnames.hasNext()) {
	    String classname = (String)classnames.next();
	    ClassPoints cp = getClassPoints(classname);
	    _out.println(classname+": "+cp.points+"/"+cp.total);
	}
    }
    private ClassPoints getClassPoints(String classname)
    {
	ClassPoints cp = (ClassPoints)_classToPoints.get(classname);
	if (cp == null) {
	    cp = new ClassPoints();
	    setClassPoints(classname, cp);
	}
	return cp;
    }
    private void setClassPoints(String classname, ClassPoints cp)
    {
	_classToPoints.put(classname, cp);
    }
    private static class ClassPoints {
	float points = 0;
	float total = 0;
    }
}
