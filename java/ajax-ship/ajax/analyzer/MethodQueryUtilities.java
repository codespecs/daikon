/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;
import java.util.*;
import ajax.util.CompactSet;

public class MethodQueryUtilities implements OpcodeConstants {
    public static void addClassMethodImplementationTargets(JBCQueryFamily family, int offset, JBCClass newClass) {
        addClassMethodImplementations(family, offset, newClass, true);
    }
    
    public static void addClassMethodImplementationSources(JBCQueryFamily family, int offset, JBCClass newClass) {
        addClassMethodImplementations(family, offset, newClass, false);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, int offset, JBCClass c, boolean asTargets) {
        addClassMethodImplementations(family, offset, c, c, new CompactSet(), asTargets);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, int offset, JBCClass newClass, JBCClass baseClass, CompactSet visitedClasses, boolean asTargets) {
        if (visitedClasses.get(newClass) == null) {
            JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
            
            visitedClasses.addUnconditionally(newClass);
            
            for (Enumeration e = newClass.getMethods(); e.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e.nextElement();
                
                if (!JBCCodeUtilities.useStaticDispatch(m)) {
                    JBCMethod impl = baseClass.getImplementationMethod(m);
               
                    if (Globals.debug && impl.isAbstract()) {
                        Globals.localError("Possible live " + impl + " in " + newClass + " is abstract!");
                    }

                    if (asTargets) {
                        family.addTargetDatum(offset, topOfStack.makeQueryFieldExpression(m), impl);
                    } else {
                        family.addSourceDatum(offset, topOfStack.makeQueryFieldExpression(m), impl);
                    }
                }
            }
            
            JBCClass superClass = newClass.getSuperClass();
            
            if (superClass != null) {
                addClassMethodImplementations(family, offset, superClass, baseClass, visitedClasses, asTargets);
            }
            
            JBCClass[] superInterfaces = newClass.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addClassMethodImplementations(family, offset, superInterfaces[i], baseClass, visitedClasses, asTargets);
            }
        }
    }

    public static void addClassMethodImplementationTargets(JBCQueryFamily family,
        ExternalCFGDefNode node, JBCClass newClass) {
        addClassMethodImplementations(family, node, newClass, true);
    }
    
    public static void addClassMethodImplementationSources(JBCQueryFamily family,
        ExternalCFGDefNode node, JBCClass newClass) {
        addClassMethodImplementations(family, node, newClass, false);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, ExternalCFGDefNode node, JBCClass c, boolean asTargets) {
        addClassMethodImplementations(family, node, c, c, new CompactSet(), asTargets);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, ExternalCFGDefNode node, JBCClass newClass, JBCClass baseClass, CompactSet visitedClasses, boolean asTargets) {
        if (visitedClasses.get(newClass) == null) {
            JBCExpression flowgraphDef = JBCExpression.makeFlowgraphDefNodeExpression();
            
            visitedClasses.addUnconditionally(newClass);
            
            for (Enumeration e = newClass.getMethods(); e.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e.nextElement();
                
                if (!JBCCodeUtilities.useStaticDispatch(m)) {
                    JBCMethod impl = baseClass.getImplementationMethod(m);
               
                    if (Globals.debug && impl.isAbstract()) {
                        Globals.localError("Possible live " + impl + " in " + newClass + " is abstract!");
                    } 

                    if (asTargets) {
                        family.addTargetDatum(node, flowgraphDef.makeQueryFieldExpression(m), impl);
                    } else {
                        family.addSourceDatum(node, flowgraphDef.makeQueryFieldExpression(m), impl);
                    }                        
                }
            }
            
            JBCClass superClass = newClass.getSuperClass();
            
            if (superClass != null) {
                addClassMethodImplementations(family, node, superClass, baseClass, visitedClasses, asTargets);
            }
            
            JBCClass[] superInterfaces = newClass.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                addClassMethodImplementations(family, node, superInterfaces[i], baseClass, visitedClasses, asTargets);
            }
        }
    }
    
    public static void addClassMethodImplementationTargets(JBCQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        addClassMethodImplementations(family, method, code, instructions, true);
    }
    
    public static void addClassMethodImplementationSources(JBCQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        addClassMethodImplementations(family, method, code, instructions, false);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions, boolean asTargets) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            addClassMethodImplementations(family, i + JBCCodeUtilities.computeOpLength(code, i),
                                c, asTargets);
                        }
                        break;
                    }
                }
            }
        }
    }

    public static void addClassMethodImplementationTargets(JBCQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        addClassMethodImplementations(family, method, fg, externalNodes, true);
    }
    
    public static void addClassMethodImplementationSources(JBCQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        addClassMethodImplementations(family, method, fg, externalNodes, false);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes, boolean asTargets) {
        addClassMethodImplementations(family, method.getContainingClass().getClassName() + "."
            + method.getMethodName(), fg, externalNodes, asTargets);
    }
    
    public static void addClassMethodImplementationTargets(JBCQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        addClassMethodImplementations(family, name, fg, externalNodes, true);
    }
    
    public static void addClassMethodImplementationSources(JBCQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        addClassMethodImplementations(family, name, fg, externalNodes, false);
    }
    
    private static void addClassMethodImplementations(JBCQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes, boolean asTargets) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode node = (ExternalCFGNewObjectDefNode)o;
                
                addClassMethodImplementations(family, node, node.getObjectClass(), asTargets);
            }
        }
    }
}
