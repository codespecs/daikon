/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;
import java.util.*;
import ajax.util.CompactSet;

public class LivenessQueryFamily extends JBCQueryFamily implements OpcodeConstants {
    private static final Object LIVE = new String("LIVE");
    
    LivenessQueryFamily(Analyzer analyzer) {
        super(analyzer);

        getAnalyzer().notifyLive("_stringconst");
        getAnalyzer().notifyLive("_magicexn");
        getAnalyzer().notifyLive("_wrapclassinitializerexn");
        getAnalyzer().notifyLive(analyzer.getWorld()
            .getSpecialClass("java.lang.System").getMethod("gc", "()V"));
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return LIVE;
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return LIVE;
    }

    private void notifyClassInstantiated(JBCClass c) {
        JBCMethod finalizerMethod = c.getInheritedMethod("finalize", "()V");
        
        if (finalizerMethod != null) {
            getAnalyzer().notifyLive(finalizerMethod);
        }
        
        notifyClassInitializerLive(c);
    }

    private void notifyClassInitializerLive(JBCClass c) {
        JBCMethod classInitializerMethod = c.getInitializerMethod();
        
        if (classInitializerMethod != null) {
            getAnalyzer().notifyLive(classInitializerMethod);
        }
        
        JBCClass superClass = c.getSuperClass();
        
        if (superClass != null) {
            notifyClassInitializerLive(superClass);
        }
    }

    protected void identifyQueryData(JBCMethod method, byte[] code, boolean[] instructions) {
        notifyClassInitializerLive(method.getContainingClass());
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            notifyClassInstantiated(c);
                        }
                        break;
                    }
                        
                    case OP_getstatic:
                    case OP_putstatic: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            notifyClassInitializerLive(f.getContainingClass());
                        }
                        break;
                    }
                        
                    case OP_invokestatic:
                    case OP_invokespecial: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (m != null) {
                            getAnalyzer().notifyLive(m);
                            notifyClassInitializerLive(m.getContainingClass());
                        }
                        break;
                    }
                        
                    case OP_invokevirtual:
                    case OP_invokeinterface: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (m != null) {
                            if (JBCCodeUtilities.useStaticDispatch(m)) {
                                getAnalyzer().notifyLive(m);
                            } else {
                                int popCount = JBCCodeUtilities.getStackPushCount(method, code, i)
                                    - JBCCodeUtilities.getStackSizeDelta(method, code, i);
                                    
                                addSourceDatum(i, JBCExpression.makeStackElemExpression(popCount - 1)
                                        .makeQueryFieldExpression(m), LIVE);
                            }
                        }
                        break;
                    }
                }
            }
        }
        
        MethodQueryUtilities.addClassMethodImplementationTargets(this, method, code, instructions);
    }

    protected void identifyQueryData(JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        notifyClassInitializerLive(method.getContainingClass());
        identifyQueryData(method.getContainingClass().getClassName() + "."
            + method.getMethodName(), fg, externalNodes);
    }
    
    protected void identifyQueryData(String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod();
                
                if (JBCCodeUtilities.useStaticDispatch(m)) {
                    if (m.isStatic()) {
                        notifyClassInitializerLive(m.getContainingClass());
                    }
                    
                    getAnalyzer().notifyLive(m);
                } else {
                    addSourceDatum(node,
                        JBCExpression.makeFlowgraphVarExpression(node.getParameters()[0])
                            .makeQueryFieldExpression(m), LIVE);
                }
            } else if (o instanceof ExternalCFGFlowgraphInvocationDefNode) {
                ExternalCFGFlowgraphInvocationDefNode node = (ExternalCFGFlowgraphInvocationDefNode)o;
                Analyzer analyzer = getAnalyzer();
                
                analyzer.notifyLive(node.getFunctionName());
            } else if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)o;
                
                notifyClassInstantiated(newNode.getObjectClass());
            } else if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode assignDefNode =
                    (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = assignDefNode.getField();

                if (field.isStatic()) {
                    notifyClassInitializerLive(field.getContainingClass());
                }
            } else if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode fieldNode = (ExternalCFGFieldDefNode)o;
                JBCField field = fieldNode.getField();

                if (field.isStatic()) {
                    notifyClassInitializerLive(field.getContainingClass());
                }
            }
        }
        
        MethodQueryUtilities.addClassMethodImplementationTargets(this, name, fg, externalNodes);
    }
    
    protected void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            getAnalyzer().notifyLive((JBCMethod)targetCookie);
        }
    }
}
