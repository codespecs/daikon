package java6170.test;

import junit.framework.TestListener;
import junit.framework.TestResult;

public interface TestRunListener extends TestListener
{
    public void init(String[] args);
    public void startRun(TestResult result);
    public void endRun(TestResult result);
}
