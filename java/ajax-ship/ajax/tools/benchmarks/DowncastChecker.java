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

public class DowncastChecker extends Benchmark implements ResultListener, OpcodeConstants {
    private Hashtable queries = new Hashtable();
    private boolean printErrorsIncrementally = false;
    private int setBounds;
    private String prefix;
    private String calledMethod;

    public DowncastChecker() {
    }

    public void notifyAnalysisComplete() {
    }
    
    public void parseOptions(Args args) {
        setBounds = args.extractIntOption("-set", -1);
        prefix = args.extractStringOption("-prefix", null);
        calledMethod = args.extractStringOption("-methodcall", null);
    }
    
    public void registerTarget(Object targetCookie) {
        JBCLocation loc = (JBCLocation)targetCookie;
        
        if (isLocationRelevant(loc)) {
            queries.put(loc, new DowncastCheckerData(loc));
        }
    }
    
    private void printReport(Writer w, JBCLocation loc, DowncastCheckerData data) throws IOException {
        w.write("At " + loc.getOffset() + " in " + loc.getMethod() + ": "
            + "bound is " + data.getBound() + "; actual " + data.getIntermediateString()
            + (data.isInBound() ? " (SAFE)\n" : "\n"));
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        JBCLocation loc = (JBCLocation)targetCookie;
        DowncastCheckerData data = (DowncastCheckerData)queries.get(loc);
        
        if (data != null) {
            boolean alreadyInError = data.isInError();
         
            data.updateResult(intermediate);
            
            if (printErrorsIncrementally && !alreadyInError && data.isInError()) {
                try {
                    Writer w = new BufferedWriter(new OutputStreamWriter(System.out));
                
                    printReport(w, loc, data);
                    w.flush();
                } catch (IOException ex) {
                }
            }
        }
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new DowncastInputTarget(), new NewObjectClassSource() };
        ResultListener[] listeners = { this };
        
        if (setBounds < 1) {
            (new ClassTracker(analyzer, specifiers, listeners)).start();
        } else {
            (new BoundedSetTracker(analyzer, specifiers, listeners, setBounds)).start();
        }
    }
    
    private boolean matchMethod(JBCMethod method, byte[] code, int offset) {
        JBCMethod callee = JBCCodeUtilities.resolveInstructionMethod(method, code, offset);
        
        if (callee == null) {
            return false;
        } else {
            String methodName = callee.getMethodName();
            
            return methodName.equals(calledMethod)
                || (callee.getContainingClass().getClassName() + "."
                    + callee.getMethodName()).toString().startsWith(calledMethod);
        }
    }
    
    private boolean isLocationRelevant(JBCLocation loc) {
        JBCMethod method = loc.getMethod();
        
        if (prefix != null &&
            !(method.getContainingClass().getClassName() + "."
                + method.getMethodName()).toString().startsWith(prefix)) {
            return false;
        } else if (calledMethod != null) {
            int offset = loc.getOffset();
            MethodData data = method.getData();
            byte[] code = data.getCode();
            boolean[] instructions = JBCCodeUtilities.getInstructionStarts(data);
            JBCMethod prevCalledMethod = null;
            
            if (offset >= 3 && instructions[offset - 3]) {
                switch (code[offset - 3] & 0xFF) {
                    case OP_invokespecial:
                    case OP_invokevirtual:
                    case OP_invokestatic:
                        if (matchMethod(method, code, offset - 3)) {
                            return true;
                        }
                }
            }
            
            if (offset >= 5 && instructions[offset - 5]
                && (code[offset - 5] & 0xFF) == OP_invokeinterface
                && matchMethod(method, code, offset - 5)) {
                return true;
            }
            
            return false;
        } else {
            return true;
        }
    }
    
    public void printReport(Writer w) throws IOException {
        int downcastsFound = 0;
        int downcastsOK = 0;
        
        for (Enumeration e = queries.keys(); e.hasMoreElements();) {
            JBCLocation loc = (JBCLocation)e.nextElement();
            DowncastCheckerData data = (DowncastCheckerData)queries.get(loc);
            boolean isError = data.isInError();
                
            downcastsFound++;
            if (!isError) {
                downcastsOK++;
            }
                
            if (!printErrorsIncrementally || !isError) {
                printReport(w, loc, data);
            }
        }
        
        w.write("Downcasts validated: " + downcastsOK + " out of " + downcastsFound + "\n");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new DowncastChecker(), "[-set <N>] [-prefix <class-prefix>] [-methodcall <method-name>]");
    }
}
