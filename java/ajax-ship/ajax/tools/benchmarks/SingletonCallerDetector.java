/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;

public class SingletonCallerDetector extends Benchmark implements ResultListener, OpcodeConstants {
    private Hashtable queryCallers = new Hashtable();
    private StaticCallRecorder staticCalls = new StaticCallRecorder();
    
    protected SingletonCallerDetector() {
    }

    public void registerTarget(Object targetCookie) {
    }

    public void notifyAnalysisComplete() {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            queryCallers.put(targetCookie, intermediate);
        } else {
            queryCallers.remove(targetCookie);
        }
    }
    
    public static String getIntermediateString(Object intermediate) {
        if (intermediate instanceof OverflowSet || intermediate instanceof CompactSet) {
            return BoundedSetTracker.toString(intermediate);
        } else {
            return intermediate.toString();
        }
    }
    
    public static boolean isSingletonCaller(Object intermediate) {
        return intermediate instanceof JBCLocation
            || intermediate instanceof ExternalLocation;
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new NewObjectMethodTarget(), new VirtualCallReceiverSource(),
            staticCalls };
        ResultListener[] listeners = { this };
        
        (new BoundedSetTracker(analyzer, specifiers, listeners, 2)).start();
    }
    
    private static void addTable(Hashtable dest, Hashtable src) {
        for (Enumeration e = src.keys(); e.hasMoreElements();) {
            Object method = e.nextElement();
            Object caller = src.get(method);
            Object curCallers = dest.get(method);
            
            if (curCallers == null) {
                dest.put(method, caller);
            } else {
                dest.put(method, BoundedSetTracker.join(caller, curCallers, Integer.MAX_VALUE));
            }
        }
    }
    
    private void addStaticCallers(Hashtable dest) {
        for (Enumeration e = staticCalls.getStaticCallLocations(); e.hasMoreElements();) {
            Location caller = (Location)e.nextElement();
            Object method = staticCalls.getStaticCallee(caller);
            Object curCallers = dest.get(method);
            
            if (curCallers == null) {
                dest.put(method, caller);
            } else {
                dest.put(method, BoundedSetTracker.join(caller, curCallers, Integer.MAX_VALUE));
            }
        }
    }
    
    static boolean isFinalizer(JBCMethod method) {
        return method.getMethodName().equals("finalize")
            && method.getMethodTypeName().equals("()V")
            && !method.isStatic();
    }
    
    public void printReport(Writer w) throws IOException {
        int singletonCallerMethods = 0;
        int multipleCallerMethods = 0;
        Hashtable callers = new Hashtable();
        
        addTable(callers, queryCallers);
        addStaticCallers(callers);
        
        for (Enumeration e = callers.keys(); e.hasMoreElements();) {
            Object callee = e.nextElement();
            
            if (callee instanceof JBCMethod) {
                JBCMethod m = (JBCMethod)callee;
                Object value = callers.get(m);
                
                if (!isFinalizer(m) && isSingletonCaller(value)) {
                    singletonCallerMethods++;
                } else {
                    multipleCallerMethods++;
                }
                
                w.write("Callers for " + m + ": " + getIntermediateString(value) + "\n");
            }
        }
        
        w.write("Singleton caller methods: "
            + GeneralBenchmark.fractionToString(singletonCallerMethods, singletonCallerMethods + multipleCallerMethods) + "\n");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new SingletonCallerDetector());
    }
}
