/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.io.*;
import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.util.*;
import ajax.Globals;

abstract class Benchmark {
    protected boolean running = false;
    
    abstract protected void configure(Analyzer analyzer);
    
    abstract protected void printReport(Writer w) throws IOException;
    
    protected void parseOptions(Args args) {
    }
    
    protected void parseArgs(Args args) {
    }
    
    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
    }
    
    protected boolean usesDynamicQueryFamilies() {
        return false;
    }

    protected void terminate() {
        synchronized (this) {
            running = false;
        }
    }
    
/**
@return true iff stopped abnormally
*/
    protected boolean work(Analyzer analyzer) {
        running = true;
        
        try {
            do {
                synchronized (this) {
                    if (!running) {
                        return true;
                    }
                }
            } while (analyzer.work());
            
            return false;
        } catch (PrematureTerminationException ex) {
            Globals.writeLog(this, "Terminated prematurely: " + ex);
            return true;
        }
    }
}
