/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.analyzer.*;

/**
Puts in field reads as targets, with the location as the cookie.
*/
public class AnyFieldWriteTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode writeNode = (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = writeNode.getField();
                
                if (!field.isStatic()) {
                    family.addTargetDatum(writeNode,
                        JBCExpression.makeFlowgraphVarExpression(writeNode.getObject()),
                        new ExternalLocation(method, writeNode));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode writeNode = (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = writeNode.getField();
                
                if (!field.isStatic()) {
                    family.addTargetDatum(writeNode,
                        JBCExpression.makeFlowgraphVarExpression(writeNode.getObject()),
                        new ExternalLocation(name, writeNode));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        JBCExpression nextTopOfStack = JBCExpression.makeStackElemExpression(1);
        JBCExpression next2TopOfStack = JBCExpression.makeStackElemExpression(2);
        JBCExpression next3TopOfStack = JBCExpression.makeStackElemExpression(3);
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_putfield:
                        family.addTargetDatum(i, nextTopOfStack, new JBCLocation(method, i));
                        break;
                        
                    case OP_aastore:
                    case OP_fastore:
                    case OP_iastore:
                    case OP_sastore:
                    case OP_castore:
                    case OP_bastore:
                        family.addTargetDatum(i, next2TopOfStack, new JBCLocation(method, i));
                        break;
                    
                    case OP_dastore:
                    case OP_lastore:
                        family.addTargetDatum(i, next3TopOfStack, new JBCLocation(method, i));
                        break;
                }
            }
        }
    }
}
