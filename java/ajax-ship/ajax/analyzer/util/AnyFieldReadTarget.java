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
public class AnyFieldReadTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode readNode = (ExternalCFGFieldDefNode)o;
                JBCField field = readNode.getField();
                
                if (!field.isStatic()) {
                    family.addTargetDatum(readNode,
                        JBCExpression.makeFlowgraphVarExpression(readNode.getObject()),
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
                        JBCExpression.makeFlowgraphVarExpression(readNode.getObject()),
                        new ExternalLocation(name, readNode));
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
        JBCExpression nextTopOfStack = JBCExpression.makeStackElemExpression(1);
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_arraylength:
                    case OP_getfield:
                        family.addTargetDatum(i, topOfStack, new JBCLocation(method, i));
                        break;
                        
                    case OP_aaload:
                    case OP_faload:
                    case OP_daload:
                    case OP_laload:
                    case OP_iaload:
                    case OP_saload:
                    case OP_caload:
                    case OP_baload:
                        family.addTargetDatum(i, nextTopOfStack, new JBCLocation(method, i));
                        break;
                }
            }
        }
    }
}
