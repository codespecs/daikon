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

public class VirtualCallResolver extends Benchmark implements ResultListener {
    private Hashtable queries = new Hashtable();
    private int setBounds;
    
    private static final Object NONE = new String("NONE");

    protected VirtualCallResolver() {
    }

    public void notifyAnalysisComplete() {
    }
    
    public void registerTarget(Object targetCookie) {
        queries.put(targetCookie, NONE);
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            queries.put(targetCookie, intermediate);
        } else {
            queries.put(targetCookie, NONE);
        }
    }
    
    public static String getIntermediateString(Object intermediate) {
        if (intermediate == NONE) {
            return "dead code";
        } else if (intermediate instanceof OverflowSet || intermediate instanceof CompactSet) {
            return BoundedSetTracker.toString(intermediate);
        } else {
            return intermediate.toString();
        }
    }
    
    public static boolean isDead(Object intermediate) {
        return intermediate == NONE;
    }
    
    public static boolean isResolved(Object intermediate) {
        return intermediate == NONE || intermediate instanceof JBCMethod;
    }
    
    private void printReport(Writer w, JBCLocation loc, boolean resolved) throws IOException {
        w.write("At " + loc.getOffset() + " in " + loc.getMethod() + ": "
            + getIntermediateString(queries.get(loc))
            + (resolved ? " (RESOLVED)\n" : " (UNRESOLVED)\n"));
    }
    
    private void printReport(Writer w, ExternalLocation loc, boolean resolved) throws IOException {
        String name = loc.getFlowgraphName();
        
        if (name == null) {
            name = loc.getMethod().toString();
        }
        w.write("At native " + loc.getNode() + " in " + name + ": "
            + getIntermediateString(queries.get(loc))
            + (resolved ? " (RESOLVED)\n" : " (UNRESOLVED)\n"));
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new NewObjectMethodSource(), new VirtualCallReceiverTarget() };
        ResultListener[] listeners = { this };
        
        (new BoundedSetTracker(analyzer, specifiers, listeners, setBounds)).start();
    }
    
    public void printReport(Writer w) throws IOException {
        int bytecodeCallSites = 0;
        int bytecodeCallSitesResolved = 0;
        int bytecodeCallSitesDead = 0;
        int externalCallSites = 0;
        int externalCallSitesResolved = 0;
        int externalCallSitesDead = 0;
        
        for (Enumeration e = queries.keys(); e.hasMoreElements();) {
            Object key = e.nextElement();
            Object value = queries.get(key);
            boolean resolved = isResolved(value);
            boolean dead = isDead(value);
            
            if (key instanceof JBCLocation) {
                if (!((JBCLocation)key).getMethod().getMethodName().equals("<clinit>")) {
                    bytecodeCallSites++;
                    if (resolved) {
                        bytecodeCallSitesResolved++;
                    }
                    if (dead) {
                        bytecodeCallSitesDead++;
                    }
                    
                    printReport(w, (JBCLocation)key, resolved);
                }
            } else if (key instanceof ExternalLocation) {
                externalCallSites++;
                if (resolved) {
                    externalCallSitesResolved++;
                }
                if (dead) {
                    externalCallSitesDead++;
                }
            
                printReport(w, (ExternalLocation)key, resolved);
            }
        }
        
        w.write("Bytecode method call sites resolved: "
            + GeneralBenchmark.fractionToString(bytecodeCallSitesResolved, bytecodeCallSites)
            + " (" + bytecodeCallSitesDead + " dead)\n"
            + "External method call sites resolved: "
            + GeneralBenchmark.fractionToString(externalCallSitesResolved, externalCallSites)
            + " (" + externalCallSitesDead + " dead)\n");
    }
    
    public void parseOptions(Args args) {
        setBounds = args.extractIntOption("-set", 1);
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new VirtualCallResolver(), "[-set <N>]");
    }
}
