/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import java.util.*;
import ajax.util.*;

class ExplanationSearch extends JBCQueryFamily implements OpcodeConstants {
    private static final Object FOUND = new String("FOUND");
    
    private ExplanationGatherer gatherer;
    private Hashtable sourcePoints = new Hashtable();
    private Hashtable targetPoints = new Hashtable();
    
    private boolean pointsUsed = false;
    
    ExplanationSearch(ExplanationGatherer gatherer, Analyzer analyzer) {
        super(analyzer);
        
        this.gatherer = gatherer;
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return FOUND;
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return FOUND;
    }
    
    private void addPoint(Hashtable t, JBCValuePoint p) {
        if (Globals.debug && pointsUsed) {
            Globals.localError("Points used early");
        }
        
        Object key = p.getLocation().getFunction();
        Object o = t.get(key);
        CompactSet set;
        
        if (o == null) {
            set = new CompactSet();
            t.put(key, set);
        } else {
            set = (CompactSet)o;
        }
        
        set.add(p);
    }
    
    void addSourcePoint(JBCValuePoint p) {
        addPoint(sourcePoints, p);
    }
    
    void addTargetPoint(JBCValuePoint p) {
        addPoint(targetPoints, p);
    }
    
    private Enumeration enumeratePoints(Hashtable t, Object key) {
        Object o = t.get(key);
        
        if (o != null) {
            return ((CompactSet)o).elements();
        } else {
            return EmptyEnumerator.get();
        }
    }
    
    private void registerPoints(Object key) {
        if (Globals.debug) {
            pointsUsed = true;
        }
        
        for (Enumeration e = enumeratePoints(sourcePoints, key); e.hasMoreElements();) {
            JBCValuePoint p = (JBCValuePoint)e.nextElement();
            
            addSourceDatum(p.getLocation(), p.getExpression(), FOUND);
        }
        
        for (Enumeration e = enumeratePoints(targetPoints, key); e.hasMoreElements();) {
            JBCValuePoint p = (JBCValuePoint)e.nextElement();
            Location l = p.getLocation();
            
            addTargetDatum(l, p.getExpression(), l);
        }
    }
    
    protected void identifyQueryData(JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        registerPoints(method);
    }
    
    protected void identifyQueryData(String name, ExternalFlowgraph fg, Vector externalNodes) {
        registerPoints(name);
    }
    
    protected void identifyQueryData(JBCMethod method, byte[] code, boolean[] instructions) {
        registerPoints(method);
        
        {   JBCType[] paramTypes = method.getMethodType().getParameterTypes();
            int wordCount = 0;
            
            for (int i = 0; i < paramTypes.length; i++) {
                JBCType paramType = paramTypes[i];
                
                addTargetDatum(0, JBCExpression.makeLocalVarExpression(wordCount),
                    new SearchMethodParam(method, i));
                
                wordCount += paramType.getWordSize();
            }
        }
        
        SearchMethodResult resultItem = new SearchMethodResult(method);
        JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
        
        for (int i = 0; i < instructions.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_ireturn:
                    case OP_areturn:
                    case OP_lreturn:
                    case OP_freturn:
                    case OP_dreturn:
                        addTargetDatum(i, topOfStack, resultItem);
                        break;
                        
                    case OP_getstatic:
                    case OP_getfield:
                        addTargetDatum(i + JBCCodeUtilities.computeOpLength(code, i),
                            topOfStack, new SearchGetField(new JBCLocation(method, i)));
                        break;
                        
                    case OP_putstatic:
                    case OP_putfield:
                        addTargetDatum(i, topOfStack, new SearchPutField(new JBCLocation(method, i)));
                        break;
                    
                    case OP_invokevirtual:
                    case OP_invokestatic:
                    case OP_invokespecial:
                    case OP_invokeinterface: {
                        Location l = new JBCLocation(method, i);
                        JBCMethod callee = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (callee != null) {
                            JBCMethodType calleeType = callee.getMethodType();
                            JBCType[] paramTypes = calleeType.getParameterTypes();
                            int wordCount = 0;
                            
                            for (int index = paramTypes.length - 1; index >= 0; index--) {
                                JBCType paramType = paramTypes[index];
                                
                                addTargetDatum(i, JBCExpression.makeStackElemExpression(wordCount),
                                    new SearchActualParam(l, index));
                                
                                wordCount += paramType.getWordSize();
                            }
                            
                            if (!calleeType.getReturnType().isEqualType(JBCType.VOID)) {
                                addTargetDatum(i + JBCCodeUtilities.computeOpLength(code, i),
                                    topOfStack, new SearchActualResult(l));
                            }
                        }
                        break;
                    }
                }
            }
        }
    }
    
    protected void updateResult(Object targetCookie, Object intermediate) {
        if (targetCookie instanceof Location) {
            gatherer.reportTermination(this);
        } else if (targetCookie instanceof SearchItem) {
            gatherer.reportResult(this, (SearchItem)targetCookie);
        }
    }
    
    protected void dispose() {
        super.dispose();
    }
}
