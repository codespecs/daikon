/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;

public class DowncastInputTarget implements DatumSpecifier, OpcodeConstants {
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        JBCExpression topOfStack = JBCExpression.makeStackElemExpression(0);
        
        for (int i = 0; i < code.length; i++) {
            if (instructions[i] && (code[i] & 0xFF) == OP_checkcast) {
                family.addTargetDatum(i, topOfStack, new JBCLocation(method, i));
            }
        }
    }
}
