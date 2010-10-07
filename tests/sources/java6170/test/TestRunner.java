package java6170.test;

import junit.framework.*;
import junit.extensions.*;
import junit.runner.*;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;

public class TestRunner extends BaseTestRunner
    implements TestRunListener
{
    private static final long RUN_TEST_TIMEOUT  = 20000;
    private static final long LOAD_TEST_TIMEOUT = 10000;
    private boolean _exiting = false;
    private boolean _failStop = false;
    private boolean _gotFail = false;
    private List _listeners = new Vector();
    private Test _test = null;
    /**
       Protect against unauthorized exit().
     */
    public TestRunner()
    {
	System.setSecurityManager(new Enforcer());
    }
    private class Enforcer extends SecurityManager
    {
	// deny calls to System.exit() unless _exiting
	public void checkExit(int status)
	{
	    if (!_exiting)
		throw new java.security.AccessControlException
		    ("System.exit() is not permitted");
	}
	// permit all other calls XXX review this!
	public void checkPermission(java.security.Permission p, Object o)
	{
	}
	public void checkPermission(java.security.Permission p)
	{
	}
    }
    private void exit(int status)
    {
	exit(status, null);
    }
    private void exit(int status, String why)
    {
	_exiting = true;
	if (why != null)
	    System.err.println(why);
	System.exit(status);
    }
    /**
       Run TestRunner.
     */
    public static void main(String[] args)
    {
	TestRunner runner = new TestRunner();
	runner.start(args);
    }
    private void start(String[] args)
    {
	init(args);
	TestResult result = new TestResult();
	startRun(result);
	getTest().run(result);
	endRun(result);
    }
    /**
       Initialize TestRunner.
     */
    public void init(String[] args)
    {
	setTest(null);
	for (int i = 0; i < args.length; i++) {
	    if (args[i].equals("--help"))
		printUsage();
	    else if (args[i].equals("--failstop"))
		_failStop = true;
	    else if (args[i].equals("--ext"))
		i = processListener(args, i+1) - 1; // for for-loop
	    else if (args[i].startsWith("-"))
		printUsage("Unrecognized option \""+args[i]+"\"");
	    else if (getTest() != null)
		printUsage("Unexpected token \""+args[i]+"\""
			   + " (only one test is class allowed)");
	    else 
		loadTest(args[i]);
	}
	if (getTest() == null)
	    printUsage("Missing <TestClassName>");
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
	buf.append("Usage: TestRunner [options] <TestClassName>\n");
	buf.append("TestRunner loads tests from TestClassName in one of two ways:\n");
	buf.append("1) if \"public static Test suite()\" defined, calls it\n");
	buf.append("2) if \"public TestClassName(String)\" defined, extracts test...() methods\n");
	buf.append("--help: print this help message\n");
	buf.append("--failstop: stop after the first error\n");
	buf.append("--ext <TestRunListener> <numArgs> arg1 ... argN\n");
	buf.append("For normal output, use \"--ext java6170.test.TestOutput 0\"");
	exit(1, buf.toString());
    }
    /**
       Load a Test from a class.
     */
    public void loadTest(String testClassName)
    {
	loadTestOrTimeout(testClassName);
	wrapTestWithTimeout(testClassName);
    }    
    /**
       Load a Test from a class or timeout trying.
     */
    private void loadTestOrTimeout(final String testClassName)
    {
	setTest(null);
	Thread thread = new Thread(new Runnable() {
		public void run()
		{
		    setTest(TestRunner.this.getTest(testClassName));
		}
	    });
	thread.setPriority(Math.max(Thread.currentThread().getPriority()-1,
				    Thread.MIN_PRIORITY));
	try {
	    thread.start();
	    thread.join(LOAD_TEST_TIMEOUT);
	} catch (InterruptedException e) {
	    exit(1, "Loading test "+testClassName+" interrupted: "+e);
	}
	
	if (thread.isAlive())
	    thread.setPriority(Thread.MIN_PRIORITY);

	if (getTest() == null)
	    exit(1, "Loading test "+testClassName+" took longer than "
		 +LOAD_TEST_TIMEOUT+"ms");

    }
    /**
       Load a test suite class.
     */
    public Test getTest(String suiteClassName) {
	Class testClass = null;
	try {
	    testClass = loadSuiteClass(suiteClassName);
	} catch (NoClassDefFoundError e) {
	    exit(1, "Class definition \""+suiteClassName+"\" not found");
	} catch (Exception e) {
	    exit(1, "Class \""+suiteClassName+"\" not found");
	}
	Method suiteMethod = null;
	try {
	    suiteMethod = testClass.getMethod(SUITE_METHODNAME,
					      new Class[0]);
	} catch (Exception e) {
	    // try to extract a test suite automatically
	    return new TestSuite(testClass);
	}
	Test test = null;
	try {
	    test = (Test)suiteMethod.invoke(null, // static method
					    new Class[0]);
	} catch (InvocationTargetException e) {
	    exit(1, "Error invoking \""+suiteClassName+"."
		 +SUITE_METHODNAME+"()\": "+e.getTargetException());
	} catch (Exception e) {
	    exit(1, "Could not invoke \""+suiteClassName+"."
		 +SUITE_METHODNAME+"()\": "+e);
	}
	return test;
    }
    /**
       Required by superclass.  Should never be called.
     */
    protected void runFailed(String message)
    {
	exit(1, "INTERNAL ERROR: "+message);
    }
    /**
       Wrap the loaded test with a TestTimeout decorator
     */
    private void wrapTestWithTimeout(final String testClassName)
    {
	try {
	    if (getTest() instanceof TestSuite)
		setTest(TestTimeout.createSuite
			((TestSuite)getTest(), RUN_TEST_TIMEOUT));
	    else
		setTest(new TestTimeout(getTest(), RUN_TEST_TIMEOUT));
	} catch (IllegalArgumentException e) {
	    exit(1, "Could not create TestTimeout for "+testClassName+": "+e);
	}
    }
    /**
       Thread-safe getter for _test
     */
    private synchronized Test getTest()
    {
	return _test;
    }
    /**
       Thread safe setter for _test
    */
    private synchronized void setTest(Test test)
    {
	_test = test;
    }
    /**
       Load a TestRunListener from a class.
     */
    private int processListener(String[] args, int i)
    {
	if (i >= args.length)
	    printUsage("Missing <TestRunListener>");
	TestRunListener listener = loadListener(args[i]);
	i++;
	if (i >= args.length)
	    printUsage("Missing <numArgs>");
	int numArgs = -1; // init to placate compiler
	try {
	    numArgs = Integer.parseInt(args[i]);
	} catch (NumberFormatException e) {
	    printUsage("Bad <numArgs>: "+e.toString());
	}
	if (numArgs < 0)
	    printUsage("<numArgs> must not be negative");
	if ((i+numArgs) >= args.length)
	    printUsage("<numArgs> too big / not enough args");
	i++;
	String[] listenerArgs = new String[numArgs];
	for (int j = 0; j < numArgs; j++)
	    listenerArgs[j] = args[i+j];
	try {
	    listener.init(listenerArgs);
	} catch (IllegalArgumentException e) {
	    exit(1, e.getMessage());
	}
	_listeners.add(listener);
	return i+numArgs;
    }
    private TestRunListener loadListener(String name)
    {
	Class theClass = null;
	try {
	    theClass = Class.forName(name);
	} catch (Exception e) {
	    exit(1, "Could not load " + name + ": " + e);
	}
	if (!TestRunListener.class.isAssignableFrom(theClass))
	    printUsage(name + " does not implement TestRunListener");
	try {
	    // XXX this call could loop forever
	    return (TestRunListener)theClass.newInstance();
	} catch (Exception e) {
	    exit(1, "Could not instantiate " + name + ": " + e);
	    return null; // not reached
	}
    }
    /**
       Implement TestRunListener.
     */
    public void startRun(TestResult result)
    {
	Iterator listeners = _listeners.iterator();
	while (listeners.hasNext()) {
	    TestRunListener listener = (TestRunListener)listeners.next();
	    result.addListener(listener);
	    listener.startRun(result);
	}
	result.addListener(this); // always last
	_gotFail = false;
    }
    public void startTest(Test test)
    {
	// ignored
    }
    public void addFailure(Test test, AssertionFailedError t)
    {
	_gotFail  = true;
	if (_failStop)
	    exit(1);
    }
    public void addError(Test test, Throwable t)
    {
	_gotFail = true;
	if (_failStop)
	    exit(1);
    }
    public void endTest(Test test)
    {
	// ignored
    }
    public void endRun(TestResult result)
    {
	Iterator listeners = _listeners.iterator();
	while (listeners.hasNext())
	    ((TestRunListener)listeners.next()).endRun(result);
	if (_gotFail)
	    exit(1);
    }
}
