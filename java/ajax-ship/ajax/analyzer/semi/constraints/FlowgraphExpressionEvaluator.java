/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import java.util.*;
import ajax.jbc.*;
import ajax.solver.Variable;
import ajax.Globals;
import ajax.analyzer.*;

class FlowgraphExpressionEvaluator {
    private Hashtable nodeInfo;
    private Hashtable varInfo;
    private Object key;
    
    FlowgraphExpressionEvaluator(Object key, Hashtable nodeInfo, Hashtable varInfo) {
        this.key = key;
        this.nodeInfo = nodeInfo;
        this.varInfo = varInfo;
    }
    
    Object getKey() {
        return key;
    }
    
    Variable getVar(ConstraintManager manager, ExternalCFGNode node, JBCExpression expression) {
        if (expression instanceof FlowgraphDefNodeExpression) {
            return getDefNodeVar((ExternalCFGDefNode)node);
        } else if (expression instanceof FlowgraphVarExpression) {
            return getVariableVar(manager, node, ((FlowgraphVarExpression)expression).getVar());
        } else if (expression instanceof JBCStaticFieldExpression) {
            Variable globalsVar = getGlobalsVar(manager, node);
            
            if (globalsVar == null) {
                return null;
            } else {
                return manager.getStaticFieldVar(globalsVar,
                    ((JBCStaticFieldExpression)expression).getField());
            }
        } else if (expression instanceof JBCStaticUserFieldExpression) {
            Variable globalsVar = getGlobalsVar(manager, node);
            
            if (globalsVar == null) {
                return null;
            } else {
                return manager.getStaticUserFieldVar(getGlobalsVar(manager, node),
                    ((JBCStaticUserFieldExpression)expression).getField());
            }
        } else {
            throw Globals.nonlocalError("Invalid expression type for a flowgraph");
        }
    }
    
    private Variable getGlobalsVar(ConstraintManager manager, ExternalCFGNode node) {
        if (manager.makeGlobalComponents) {
            return FlowgraphConstraintGenerator.pushToRoot(node,
                JBCMethodGlobalsArgComponent.get(), nodeInfo, manager.getSolver());
        } else {
            return manager.getGlobalsVar();
        }
    }
    
    private Variable getVariableVar(ConstraintManager manager, ExternalCFGNode node,
        ExternalCFGVariable variable) {
        return FlowgraphConstraintGenerator.traceNodeDefAndPropagate(node, variable,
            nodeInfo, varInfo, manager.getSolver());
    }
    
    private Variable getDefNodeVar(ExternalCFGDefNode node) {
        if (Globals.debug && nodeInfo == null) {
            Globals.localError("Illegal operation in compressed state!");
        }
        
        ExternalCFGNodeInfo info = (ExternalCFGNodeInfo)nodeInfo.get(node);
        
        if (info == null || info.defVars == null || info.defVars.length < 1) {
            return null;
        } else {
            return info.defVars[0];
        }
    }
    
    void compress() {
    }
    
    void uncompress() {
    }
}
