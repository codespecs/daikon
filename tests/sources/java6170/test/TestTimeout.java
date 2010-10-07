package java6170.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import junit.framework.TestResult;
import junit.framework.TestListener;
import junit.framework.AssertionFailedError;
import junit.extensions.TestDecorator;
import java.util.Enumeration;
import java.util.Stack;

/**
   A TestDecorator that times out if the given Test runs longer than a
   specified number of milliseconds.
   @author Sameer Ajmani (ajmani@mit.edu) */
public class TestTimeout extends TestDecorator implements TestListener
{
    private long _millis; // the timeout in milliseconds
    private int _numRun;  // the number of tests run before timeout
    /**
       Constructs a Test that times out if the given Test runs longer than
       millis ms.  */
    public TestTimeout(final Test test, final long millis)
    {
        super(test);
        _millis = millis;
    }
    /**
       Constructs a TestSuite from the given TestSuite whose tests time out
       if they run longer than millis ms.
       @throws IllegalArgumentException if the suite contains itself
    */
    public static TestSuite createSuite(final TestSuite suite,
                                        final long millis)
    {
        return createSuite(suite, millis, new Stack());
    }
    private static TestSuite createSuite(final TestSuite suite,
                                         final long millis,
                                         final Stack stack)
    {
        // fail if suite contains itself (should never happen)
        if (stack.contains(suite))
            throw new IllegalArgumentException
                ("suite "+suite.getClass().getName()+" contains itself");
        TestSuite newSuite = new TestSuite();
        Enumeration tests = suite.tests();
        // wrap each test in the suite with a TestTimeout decorator
        while (tests.hasMoreElements()) {
            Test test = (Test)tests.nextElement();
            if (test instanceof TestSuite) {
                stack.push(suite);
                newSuite.addTest(createSuite((TestSuite)test, millis, stack));
                stack.pop();
            } else {
                newSuite.addTest(new TestTimeout(test, millis));
            }
        }
        return newSuite;
    }

    /**
       Runs the underlying Test for up to _millis ms.  If the Test times
       out, adds an error to the given TestResult. */
    public void run(final TestResult result)
    {   
        _numRun = 0;
        result.addListener(this);
        runUntilTimeout(result);
        result.removeListener(this);
        checkResult(result);
    }
    /**
       Runs the underlying Test for up to _millis ms.  If testing is
       interrupted, adds an error to the given TestResult. */
    private void runUntilTimeout(final TestResult result)
    {
        Thread thread = new Thread(new Runnable() {
                public void run()
                {
                    TestTimeout.this.getTest().run(result);
                }
            });
        // ensure the test thread can't lock up our thread
        thread.setDaemon(true);
        thread.setPriority(Math.max(Thread.currentThread().getPriority()-1,
                                    Thread.MIN_PRIORITY));
        thread.start();
        try {
            thread.join(_millis);
        } catch (InterruptedException e) {
            result.addError(getTest(), e);
        }
        if (thread.isAlive())
            thread.setPriority(Thread.MIN_PRIORITY);
    }

    // XXX Kludge to make the anonymous Runnable in runUntilTimeout
    // link correctly under IBM's JVM on Linux.  We may be able to
    // remove this now (the call used to be just getTest().run(...)),
    // we should try removing it and testing it eventually.
    public Test getTest()
    {
	return super.getTest();
    }
  
    /**
       Checks that all the TestCases in the underlying Test were run.  If
       not, adds an error to the given TestResult.  */
    private synchronized void checkResult(final TestResult result)
    {
        int numTests = getTest().countTestCases();
        if (_numRun < numTests) {
            StringBuffer message = new StringBuffer
                ("Test time exceeded ").append(_millis).append("ms");
            if (numTests > 1) {
                message.append(": ").append(_numRun)
                    .append(" out of ").append(numTests)
                    .append(" tests run");
            }
            result.addError(getTest(),
                            new InterruptedException(message.toString()));
        }
    }
    public synchronized void endTest(final Test test)
    {
        _numRun++;
    }
    public synchronized void startTest(final Test test)
    {
        // ignored
    }
    public synchronized void addError(final Test test, final Throwable t)
    {
        // ignored
    }
    public synchronized void addFailure(final Test test,
                                        final AssertionFailedError t)
    {
        // ignored
    }
}
