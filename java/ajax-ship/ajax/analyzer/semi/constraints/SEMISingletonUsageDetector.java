/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.Globals;
import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;

class SEMISingletonUsageDetector implements ResultListener, DatumSpecifier, OpcodeConstants {
    private Hashtable queryCallers = new Hashtable();
    private StaticCallRecorder staticCalls;
    private Hashtable callers = null;
    private boolean doneResolution = false;
    private Hashtable classInstantiations = new Hashtable();
    private boolean useAnalysis;
    private Analyzer analyzer;
    
    private static final Object ONE = new String("ONE");
    private static final Object MANY = new String("MANY");

/** Return value from getMethodCallStatus: the call site cannot call this method */
    public static final int CALL_DEAD             = 0;
/** Return value from getMethodCallStatus: method may be called at the site, no other call sites possible */
    public static final int CALL_SINGLE_CALLER    = 1;
/** Return value from getMethodCallStatus: method may be called at the site, other call sites possible */
    public static final int CALL_MULTIPLE_CALLERS = 2;
    
    private void addClassInstantiation(JBCClass c) {
        Object o = classInstantiations.get(c);
        
        if (o == null) {
            classInstantiations.put(c, ONE);
        } else if (o == ONE) {
            classInstantiations.put(c, MANY);
        }
    }
    
    public SEMISingletonUsageDetector(ConstraintManager manager, boolean useAnalysis) {
        this.useAnalysis = useAnalysis;
    }

    public void registerTarget(Object targetCookie) {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (doneResolution) {
            Globals.nonlocalError("Sorry, SEMIUselessFieldDetector does not support incremental update at this time!");
        }
        
        if (intermediate != null) {
            queryCallers.put((JBCMethod)targetCookie, intermediate);
        } else {
            queryCallers.remove((JBCMethod)targetCookie);
        }
    }
    
    void configure(Analyzer analyzer) {
        this.analyzer = analyzer;
        
        if (useAnalysis) {
            staticCalls = new StaticCallRecorder();
            
            DatumSpecifier[] specifiers = { new NewObjectMethodTarget(), new VirtualCallReceiverSource(),
                staticCalls, this };
            ResultListener[] listeners = { this };
            
            (new SingletonTracker(analyzer, specifiers, listeners)).start();
        }
    }
    
    static boolean isFinalizer(JBCMethod method) {
        return method.getMethodName().equals("finalize")
            && method.getMethodTypeName().equals("()V")
            && !method.isStatic();
    }
    
    int getMethodCallStatus(JBCMethod method, Location checkLocation) {
        if (!doneResolution) {
            doneResolution = true;
        }
        
        if (!useAnalysis || isFinalizer(method)) {
            return CALL_MULTIPLE_CALLERS;
        } else {
            if (callers == null) {
                callers = new Hashtable();
                
                for (Enumeration e = queryCallers.keys(); e.hasMoreElements();) {
                    Object key = e.nextElement();

                    addPair(callers, key, queryCallers.get(key));
                }
                
                for (Enumeration e = staticCalls.getStaticCallLocations(); e.hasMoreElements();) {
                    Location l = (Location)e.nextElement();

                    addPair(callers, staticCalls.getStaticCallee(l), l);
                }
            }
            
            Object o = callers.get(method);
            
            if (o == null) {
                return CALL_DEAD;
            } else if (o instanceof Location) {
                if (o.equals(checkLocation)) {
                    return CALL_SINGLE_CALLER;
                } else {
                    if (Globals.debug && analyzer.isCodeLive(checkLocation.getFunction())) {
                        Globals.localError("Unknown call site for method: " + method + " at " + checkLocation);
                    }
                    return CALL_DEAD;
                }
            } else {
                return CALL_MULTIPLE_CALLERS;
            }   
        }
    }
    
    boolean isClassInstantiatedOnce(JBCClass c) {
        doneResolution = true;
        
        return useAnalysis && classInstantiations.get(c) != MANY;
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode node = (ExternalCFGNewObjectDefNode)o;
                
                addClassInstantiation(node.getObjectClass());
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode node = (ExternalCFGNewObjectDefNode)o;
                
                addClassInstantiation(node.getObjectClass());
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            addClassInstantiation(c);
                        }
                        break;
                    }
                }
            }
        }
    }
    
    private static void addPair(Hashtable dest, Object key, Object value) {
        Object curValue = dest.get(key);
            
        if (curValue == null) {
            dest.put(key, value);
        } else {
            dest.put(key, SingletonTracker.join(value, curValue));
        }
    }
}
