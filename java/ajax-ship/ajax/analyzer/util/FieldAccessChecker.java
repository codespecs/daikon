/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.util.*;

public class FieldAccessChecker implements ResultListener, DatumSpecifier, OpcodeConstants {
    private CompactSet readWriteNonstaticFields = new CompactSet();
    private CompactSet readStaticFields = new CompactSet();
    private CompactSet writeStaticFields = new CompactSet();

    public FieldAccessChecker() {
    }
    
    public void registerTarget(Object targetCookie) {
    }
    
    public boolean isFieldUsed(JBCField f) {
        return f.isStatic() ? readStaticFields.get(f) != null && writeStaticFields.get(f) != null
            : readWriteNonstaticFields.get(f) != null;
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            readWriteNonstaticFields.add((JBCField)targetCookie);
        } else {
            readWriteNonstaticFields.remove((JBCField)targetCookie);
        }
    }
    
    public void notifyAnalysisComplete() {
    }
    
    private void identifyQueryData(Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode readNode = (ExternalCFGFieldDefNode)o;
                JBCField field = readNode.getField();
                
                if (field.isStatic()) {
                    readStaticFields.add(field);
                }
            } else if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode writeNode = (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = writeNode.getField();
                
                if (!field.isStatic()) {
                    writeStaticFields.add(field);
                }
            }
        }
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_getstatic: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            readStaticFields.add(f);
                        }
                        break;
                    }
                    
                    case OP_putstatic: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            writeStaticFields.add(f);
                        }
                        break;
                    }
                }
            }
        }
    }
}
