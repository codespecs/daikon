/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.jbc.util.*;

public class AnyMethodInvocationOnThisTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod();
                
                if (!m.isStatic()) {
                    family.addTargetDatum(node,
                        JBCExpression.makeFlowgraphVarExpression(node.getParameters()[0]),
                        new ExternalLocation(method, node));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod();
                
                if (!m.isStatic()) {
                    family.addTargetDatum(node,
                        JBCExpression.makeFlowgraphVarExpression(node.getParameters()[0]),
                        new ExternalLocation(name, node));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_invokespecial:
                    case OP_invokevirtual:
                    case OP_invokeinterface: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (m != null) {
                            int popCount = JBCCodeUtilities.getStackPushCount(method, code, i)
                                - JBCCodeUtilities.getStackSizeDelta(method, code, i);
                                        
                            family.addTargetDatum(i, JBCExpression.makeStackElemExpression(popCount - 1),
                                new JBCLocation(method, i));
                        }
                        break;
                    }
                }
            }
        }
    }
}
