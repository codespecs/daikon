/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.jbc.util.*;

public class NewObjectTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        JBCExpression defNodeExpression = JBCExpression.makeFlowgraphDefNodeExpression();
        
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)o;
                
                family.addTargetDatum(newNode, defNodeExpression, new ExternalLocation(method, newNode));
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        JBCExpression defNodeExpression = JBCExpression.makeFlowgraphDefNodeExpression();
        
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)o;
                
                family.addTargetDatum(newNode, defNodeExpression, new ExternalLocation(name, newNode));
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
                    case OP_multianewarray:
                        family.addTargetDatum(i + JBCCodeUtilities.computeOpLength(code, i),
                            topOfStack,
                            new JBCLocation(method, i));
                }
            }
        }
    }
}
