/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.analyzer.*;

public class NewObjectClassSource implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(family, externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(family, externalNodes);
    }
    
    private static void identifyQueryData(IndirectionQueryFamily family, Vector externalNodes) {
        JBCExpression defNodeExpression = JBCExpression.makeFlowgraphDefNodeExpression();
        
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)o;
                
                family.addSourceDatum(newNode, defNodeExpression, newNode.getObjectClass());
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            family.addSourceDatum(i + JBCCodeUtilities.computeOpLength(code, i),
                                topOfStack, c);
                        }
                        break;
                    }
                }
            }
        }
    }
}
