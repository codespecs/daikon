/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import ajax.solver.*;
import ajax.Globals;

/**
This class resolves virtual method calls for SEMI. It can be used with any
analysis. Currently it does not support incremental update.
*/
class SEMIVirtualCallResolver implements ResultListener {
    private Hashtable queries = new Hashtable();
    private boolean doneResolution = false;
    private ConstraintManager manager;
    private boolean haveAnalysis;
    private CompactSet methodsInJustOneSet = null;
    private Hashtable calleeSetRefCounts = null;
    private Hashtable calleeSetVarCache = null;

    private static final Object NONE = new String("NONE");
    
    SEMIVirtualCallResolver(ConstraintManager manager, boolean haveAnalysis) {
        this.manager = manager;
        this.haveAnalysis = haveAnalysis;
    }
    
    void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new VirtualCallReceiverTarget(), new NewObjectMethodSource() };
        ResultListener[] listeners = { this };
        
        if (manager.isVirtualCallingNondeterministic()) {
            (new UnboundedSetTracker(analyzer, specifiers, listeners)).start();

            calleeSetRefCounts = new Hashtable();
            calleeSetVarCache = new Hashtable();
        } else {
            (new SingletonTracker(analyzer, specifiers, listeners)).start();
        }
    }

    public void registerTarget(Object targetCookie) {
        queries.put(targetCookie, NONE);
    }
    
    public void notifyAnalysisComplete() {
    }
    
    private void changeRefCount(Object intermediate, int delta) {
        if (intermediate instanceof HashableCompactSet) {
            Object o = calleeSetRefCounts.get(intermediate);
            
            if (o != null) {
                int[] v = (int[])o;
                int newValue = v[0] + delta;
                
                if (newValue == 0) {
                    calleeSetRefCounts.remove(intermediate);
                } else {
                    v[0] = newValue;
                }
            } else {
                int[] v = { delta };
                
                calleeSetRefCounts.put(intermediate, v);
            }
        }
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (doneResolution) {
            Globals.nonlocalError("Sorry, SEMIVirtualCallResolver does not support incremental update at this time!");
        }
        
        if (manager.isVirtualCallingNondeterministic()) {
            changeRefCount(queries.get(targetCookie), -1);
            changeRefCount(intermediate, 1);
        }
        
        if (intermediate != null) {
            queries.put(targetCookie, intermediate);
        } else {
            queries.put(targetCookie, NONE);
        }
    }
    
    void resolveCall(InvocationContext context, JBCMethod m, int offset,
        JBCMethod method, Variable objVar, Variable callVar) {
        resolveCall(context, new JBCLocation(m, offset), method, objVar, callVar);
    }
    
    void resolveCall(InvocationContext context, JBCMethod m, ExternalCFGNode node,
        JBCMethod method, Variable objVar, Variable callVar) {
        resolveCall(context, new ExternalLocation(m, node), method, objVar, callVar);
    }
    
    void resolveCall(InvocationContext context, String name, ExternalCFGNode node,
        JBCMethod method, Variable objVar, Variable callVar) {
        resolveCall(context, new ExternalLocation(name, node), method, objVar, callVar);
    }
    
    private InstanceLabel makeInstanceLabel(InvocationContext context, Location location) {
        if (location instanceof JBCLocation) {
            JBCLocation loc = (JBCLocation)location;
                    
            return JBCMethodInvocationInstance.get(context,
                loc.getMethod(), loc.getOffset());
        } else {
            ExternalCFGNode node = ((ExternalLocation)location).getNode();
                    
            return new ExternalCFGMethodInvocationInstance(
                (ExternalCFGMethodInvocationDefNode)node);
        }
    }
    
    private boolean isMethodInJustOneSet(JBCMethod m) {
        if (methodsInJustOneSet == null) {
            CompactSet methodsInManySets = new CompactSet();
            
            methodsInJustOneSet = new CompactSet();
            
            for (Enumeration e = calleeSetRefCounts.keys(); e.hasMoreElements();) {
                for (Enumeration e2 = ((HashableCompactSet)e.nextElement()).elements(); e2.hasMoreElements();) {
                    JBCMethod callee = (JBCMethod)e2.nextElement();
                    
                    if (methodsInManySets.get(callee) == null) {
                        if (methodsInJustOneSet.remove(callee) == null) {
                            methodsInJustOneSet.addUnconditionally(callee);
                        } else {
                            methodsInManySets.addUnconditionally(callee);
                        }
                    }
                }
            }
        }
        
        return methodsInJustOneSet.get(m) != null;
    }
    
    private void resolveCall(InvocationContext context, Location location,
        JBCMethod method, Variable objVar, Variable callVar) {
        World solver = manager.getSolver();
        Object intermediate = haveAnalysis ? queries.get(location) : null;
        
        doneResolution = true;
        
        // Intermediate can be 'null', when the call occurs in code that is known to be
        // dead by the prephase analysis
        if (intermediate == NONE || intermediate == null) {
            /* no receiver ... do nothing, this code must be dead */
        } else if (intermediate instanceof JBCMethod) {
            /* single receiver, paydirt */
            JBCMethod calledMethod = (JBCMethod)intermediate;
            SEMISingletonUsageDetector singletonDetector = manager.getSingletonUsageDetector();
            int callStatus = singletonDetector.getMethodCallStatus(calledMethod, location);
            
            if (callStatus == singletonDetector.CALL_SINGLE_CALLER) {
                manager.getMethodVar(context, calledMethod).makeEqual(solver, callVar);
            } else if (callStatus != singletonDetector.CALL_DEAD) {
                InstanceLabel instance = makeInstanceLabel(context, location);
                
                manager.getMethodVar(context, calledMethod).getInstance(solver, instance)
                    .makeEqual(solver, callVar);
            }
        } else if (manager.isVirtualCallingNondeterministic() && intermediate instanceof HashableCompactSet) {
            HashableCompactSet set = (HashableCompactSet)intermediate;
            int vRefCount = ((int[])calleeSetRefCounts.get(set))[0];
            Variable v = (Variable)calleeSetVarCache.get(set);
            
            if (v == null) {
                v = new Variable(solver);
                
                for (Enumeration e = set.elements(); e.hasMoreElements();) {
                    JBCMethod m = (JBCMethod)e.nextElement();
                    Variable mCall = manager.getMethodVar(manager.getRootInvocationContext(), m);
                    
                    if (!isMethodInJustOneSet(m)) {
                        mCall = mCall.getInstance(solver, new NondeterministicSetInstance(set));
                    }
                    
                    mCall.makeEqual(solver, v);
                }
                
                calleeSetVarCache.put(set, v);
            } else if (Globals.debug && vRefCount < 2) {
                Globals.localError("Inconsistent refcounting for target method sets!");
            }
            
            if (vRefCount == 1) {
                v.makeEqual(solver, callVar);
            } else {
                InstanceLabel instance = makeInstanceLabel(context, location);
                
                v.getInstance(solver, instance).makeEqual(solver, callVar);
            }
        } else {
            if (Globals.debug && manager.isVirtualCallingNondeterministic()) {
                Globals.localError("Should never use virtual method slots with nondeterministic calling on");
            }
                
            /* multiple receivers or unknown */
            manager.makeNonstaticMethod(Variable.DMODE, context, objVar, method)
                .makeEqual(solver, callVar);
                
        }
    }
}
