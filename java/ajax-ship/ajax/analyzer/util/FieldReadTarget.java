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
public class FieldReadTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode readNode = (ExternalCFGFieldDefNode)o;
                JBCField field = readNode.getField();
                
                if (!field.isStatic()) {
                    family.addTargetDatum(readNode,
                        JBCExpression.makeFlowgraphVarExpression(readNode.getObject())
                            .makeQueryFieldExpression(field),
                        new ExternalLocation(method, readNode));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode readNode = (ExternalCFGFieldDefNode)o;
                JBCField field = readNode.getField();
                
                if (!field.isStatic()) {
                    family.addTargetDatum(readNode,
                        JBCExpression.makeFlowgraphVarExpression(readNode.getObject())
                            .makeQueryFieldExpression(field),
                        new ExternalLocation(name, readNode));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i] && (code[i] & 0xFF) == OP_getfield) {
                JBCField field = JBCCodeUtilities.resolveInstructionField(method, code, i);
                
                if (field != null) {
                    family.addTargetDatum(i, 
                        topOfStack.makeQueryFieldExpression(field),
                        new JBCLocation(method, i));
                }
            }
        }
    }
}
