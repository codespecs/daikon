package java6170.test;

import junit.framework.*;
import junit.extensions.*;
import java.util.*;
import java.io.*;

public class TestOutput implements TestRunListener
{
    private PrintStream _out = System.out;
    private boolean _gotFail      = false;
    private boolean _noPass       = false;
    private boolean _printStack   = false;
    private boolean _filterStack  = false;
    private boolean _printSummary = false;
    private Map _nameToResult = null;
    private TestResult _totalResult = null;
    public void init(String[] args)
    {
	for (int i = 0; i < args.length; i++) {
	    if (args[i].equals("--help"))
		printUsage();
	    else if (args[i].equals("--nopass"))
		_noPass = true;
	    else if (args[i].equals("--printstack"))
		_printStack = true;
	    else if (args[i].equals("--filterstack"))
		_filterStack = true;
	    else if (args[i].equals("--printsummary"))
		_printSummary = true;
	    else if (args[i].startsWith("-"))
		printUsage("Unrecognized option: "+args[i]);
	    else {
		try {
		    _out = new PrintStream(new FileOutputStream(args[i]));
		} catch (FileNotFoundException e) {
		    printUsage(e.toString());
		}
	    }

	}
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
	buf.append("Usage: TestOutput [options] [outputFileName]\n");
	buf.append("--help: print this help message\n");
	buf.append("--nopass: don't print passing results\n");
	buf.append("--printstack: print stack trace for errors\n");
	buf.append("--filterstack: exclude JUnit frames from stack traces\n");
	buf.append("--printsummary: print a summary of test results\n");
	buf.append("outputFileName: defaults to standard output");
	throw new IllegalArgumentException(buf.toString());
    }
    public void startRun(TestResult result)
    {
	_totalResult = new TestResult();
	_nameToResult = new LinkedHashMap();
    }
    public void startTest(Test test)
    {
	_totalResult.startTest(test);
	getResult(test).startTest(test);
	_gotFail = false;
    }
    public void addFailure(Test test, AssertionFailedError t)
    {
	_totalResult.addFailure(test, t);
	getResult(test).addFailure(test, t);
	processFail(test, t);
    }
    public void addError(Test test, Throwable t)
    {
	_totalResult.addError(test, t);
	getResult(test).addError(test, t);
	processFail(test, t);
    }
    public void endTest(Test test)
    {
	_totalResult.endTest(test);
	getResult(test).endTest(test);
	if (!_gotFail)
	    processPass(test);
    }
    public void endRun(TestResult result)
    {
	if (_printSummary) {
	    // print summary for each test class
	    Iterator names = _nameToResult.keySet().iterator();
	    while (names.hasNext()) {
		String name = (String)names.next();
		printSummary(name, (TestResult)_nameToResult.get(name));
	    }
	    // print total summary
	    printSummary("ALL TESTS", _totalResult);
	}
    }
    private void printSummary(String name, TestResult result)
    {
	int totalCount = result.runCount();
	int errorCount = result.errorCount();
	int failCount  = result.failureCount();
	int passCount  = totalCount - (failCount + errorCount);
	_out.println("SUMMARY FOR "+name);
	_out.println("Pass : "+passCount
		     +" ("+percent(passCount, totalCount)+")");
	_out.println("Fail : "+failCount
		     +" ("+percent(failCount, totalCount)+")");
	_out.println("Error: "+errorCount
		     +" ("+percent(errorCount, totalCount)+")");
	_out.println("Total: "+totalCount);
    }
    private String percent(int count, int total)
    {
	return java.text.NumberFormat.getPercentInstance().format
	    ((double)count/(double)total);
    }
    private void processPass(Test test)
    {
	if (_noPass) return;
	_out.print("PASS: ");
	_out.println(test);
    }
    private void processFail(Test test, Throwable t)
    {
	_gotFail = true;
	_out.print("FAIL: ");
	_out.println(test);
	if (_printStack) {
	    if (_filterStack)
		_out.print(filteredStack(t));
	    else
		t.printStackTrace(_out);
	} else {
	    _out.println(t);
	}
    }
    private String filteredStack(Throwable t)
    {
	StringWriter trace = new StringWriter();
	t.printStackTrace(new PrintWriter(trace));
	BufferedReader reader = new BufferedReader
	    (new StringReader(trace.toString()));
	StringWriter newTrace = new StringWriter();
	try {
	    while (true) {
		String line = reader.readLine();
		if (line == null) break;
		if (line.indexOf("at junit.") >= 0) continue;
		newTrace.write(line);
		newTrace.write('\n');
	    }
	} catch (IOException e) {
	    // we don't want to break the run because of this
	    newTrace.write("INTERNAL ERROR: TestOutput.getStack(): ");
	    newTrace.write(e.toString());
	    newTrace.write('\n');
	}
	return newTrace.toString();
    }
    /**
       Get the test result (collector) for the given test's class.
       Create a new result if none exists for the test's class.
    */
    private TestResult getResult(Test test)
    {
	String key = test.getClass().getName();
	TestResult result = (TestResult)_nameToResult.get(key);
	if (result == null) {
	    result = new TestResult();
	    _nameToResult.put(key, result);
	}
	return result;
    }
}
