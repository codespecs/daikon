/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.rta;

import ajax.analyzer.*;
import ajax.util.*;
import ajax.jbc.*;
import java.util.*;
import ajax.Globals;

class RTAFlowgraphTypeInfo {
    private Hashtable nodesToVarsToDefs = new Hashtable();
    private int maxNumDefinedVars = 0;
    private Object resolver;
    private boolean distinctPrimitives;
    
    private RTAFlowgraphTypeInfo(Object resolver, ExternalFlowgraph fg,
        boolean distinctPrimitives) {
        this.resolver = resolver;
        this.distinctPrimitives = distinctPrimitives;
        
        initDefs(fg);
    }

    RTAFlowgraphTypeInfo(JBCMethod resolver, ExternalFlowgraph fg,
        boolean distinctPrimitives) {
        this((Object)resolver, fg, distinctPrimitives);
    }
    
    RTAFlowgraphTypeInfo(JBCWorld resolver, ExternalFlowgraph fg,
        boolean distinctPrimitives) {
        this((Object)resolver, fg, distinctPrimitives);
    }
    
    private void initDefs(ExternalFlowgraph fg) {
        for (Enumeration e = ExternalFlowgraphUtilities.getDefNodes(fg).elements(); e.hasMoreElements();) {
            ExternalCFGDefNode defNode = (ExternalCFGDefNode)e.nextElement();
            ExternalCFGVariable[] vars = defNode.getDefinedVariables();
            
            if (maxNumDefinedVars < vars.length) {
                maxNumDefinedVars = vars.length;
            }
            
            for (int i = 0; i < vars.length; i++) {
                addDefToSuccessors(vars[i], defNode, defNode);
            }
        }
    }
    
    private void addDefToSuccessors(ExternalCFGVariable v, ExternalCFGDefNode defNode, ExternalCFGNode node) {
        for (Enumeration e = node.getSuccessors(); e.hasMoreElements();) {
            ExternalCFGNode successor = (ExternalCFGNode)e.nextElement();
            Hashtable varsToDefs = (Hashtable)nodesToVarsToDefs.get(successor);
            
            if (varsToDefs == null) {
                varsToDefs = new Hashtable();
                nodesToVarsToDefs.put(successor, varsToDefs);
            }
            
            CompactSet defs = (CompactSet)varsToDefs.get(v);
            
            if (defs == null) {
                defs = new CompactSet();
                varsToDefs.put(v, defs);
            }
            
            if (defs.get(defNode) == null) {
                boolean killed = false;
                
                defs.addUnconditionally(defNode);
                
                if (successor instanceof ExternalCFGDefNode) {
                    ExternalCFGVariable[] kills = ((ExternalCFGDefNode)successor).getDefinedVariables();
                    
                    for (int i = 0; i < kills.length && !killed; i++) {
                        if (kills[i].equals(v)) {
                            killed = true;
                        }
                    }
                }
                
                if (!killed) {
                    addDefToSuccessors(v, defNode, successor);
                }
            }
        }
    }
    
    private Object convertType(JBCType t) {
        if (t instanceof JBCObjectType) {
            return ((JBCObjectType)t).getClassDef();
        } else if (t.isEqualType(JBCType.INT)) {
            /* this did a "weak check", i.e. it's true for boolean, byte, char, short also */
            return JBCType.INT;
        } else if (distinctPrimitives) {
            return t;
        } else if (t.equals(RTA.TOP)) {
            return t;
        } else {
	    return JBCType.INT;
	}
    }
    
    private void addTypesFromDefNode(ExternalCFGVariable v, ExternalCFGDefNode defNode, CompactSet result, CompactSet[] visitedDefNodes) {
        int varIndex = 0;
        ExternalCFGVariable[] definedVars = defNode.getDefinedVariables();
        
        for (varIndex = 0; !definedVars[varIndex].equals(v); varIndex++) {
        }
        
        if (visitedDefNodes[varIndex] == null) {
            visitedDefNodes[varIndex] = new CompactSet();
        }
        
        if (visitedDefNodes[varIndex].get(defNode) == null) {
            visitedDefNodes[varIndex].addUnconditionally(defNode);
            
            if (defNode instanceof ExternalCFGCatchDefNode) {
                result.add(((ExternalCFGCatchDefNode)defNode).getCatchClass());
            } else if (defNode instanceof ExternalCFGFieldDefNode) {
                result.add(convertType(
                        ((ExternalCFGFieldDefNode)defNode).getField().getFieldType()));
            } else if (defNode instanceof ExternalCFGFieldAssignmentDefNode) {
                addTypesFromDefinedVar(
                    ((ExternalCFGFieldAssignmentDefNode)defNode).getValue(),
                    defNode, result, visitedDefNodes);
            } else if (defNode instanceof ExternalCFGMethodInvocationDefNode) {
                if (varIndex == 0) {
                    result.add(convertType(
                            ((ExternalCFGMethodInvocationDefNode)defNode)
                            .getMethod().getMethodType().getReturnType()));
                } else {
                    result.add(RTA.TOP);
                }
            } else if (defNode instanceof ExternalCFGNewObjectDefNode) {
                result.add(((ExternalCFGNewObjectDefNode)defNode).getObjectClass());
            } else if (defNode instanceof ExternalCFGParameterDefNode) {
                if (resolver instanceof JBCMethod) {
                    result.add(convertType(
                            ((JBCMethod)resolver).getMethodType()
                                .getParameterTypes()[
                                    ((ExternalCFGParameterDefNode)defNode).getParameterIndex()]));
                } else {
                    result.add(RTA.TOP);
                }
            } else if (defNode instanceof ExternalCFGChoiceDefNode) {
                ExternalCFGVariable[] vars = ((ExternalCFGChoiceDefNode)defNode).getChoiceValues();
                
                if (vars.length == 0) {
                    result.add(RTA.TOP);
                } else {
                    for (int i = 0; i < vars.length; i++) {
                        addTypesFromDefinedVar(vars[i], defNode, result, visitedDefNodes);
                    }
                }
            } else {
                result.add(RTA.TOP);
            }
        }
    }
    
    private void addTypesFromDefinedVar(ExternalCFGVariable v, ExternalCFGNode node, CompactSet result, CompactSet[] visitedDefNodes) {
        Hashtable varsToDefs = (Hashtable)nodesToVarsToDefs.get(node);
        
        if (varsToDefs != null) {
            CompactSet defs = (CompactSet)varsToDefs.get(v);
            
            if (defs != null) {
                for (Enumeration e = defs.elements(); e.hasMoreElements();) {
                    addTypesFromDefNode(v, (ExternalCFGDefNode)e.nextElement(), result, visitedDefNodes);
                }
            }
        }
    }

    Enumeration getTypesAt(ExternalCFGNode node, JBCExpression expression) {
        CompactSet result = new CompactSet();
        
        if (expression instanceof FlowgraphDefNodeExpression) {
            ExternalCFGDefNode defNode = (ExternalCFGDefNode)node;
            
            addTypesFromDefNode(defNode.getDefinedVariables()[0], defNode, result, new CompactSet[maxNumDefinedVars]);
        } else {
            if (Globals.debug && !(expression instanceof FlowgraphVarExpression)) {
                Globals.nonlocalError("Invalid expression type: " + expression);
            }
            
            addTypesFromDefinedVar(((FlowgraphVarExpression)expression).getVar(), node, result, new CompactSet[maxNumDefinedVars]);
        }
        
        return result.elements();
    }
}
