/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import java.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;

public class UselessFieldDetector extends Benchmark implements DatumSpecifier, OpcodeConstants {
    private Hashtable fieldAccessCounts = new Hashtable();
    private FieldAccessChecker checker = new FieldAccessChecker();
    
    protected UselessFieldDetector() {
    }
    
    private void addCount(JBCField f) {
        Object o = fieldAccessCounts.get(f);
        
        if (o == null) {
            fieldAccessCounts.put(f, new Integer(1));
        } else {
            fieldAccessCounts.put(f, new Integer(((Integer)o).intValue() + 1));
        }
    }

    private void identifyQueryData(Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode readNode = (ExternalCFGFieldDefNode)o;
                JBCField field = readNode.getField();
                
                addCount(field);
            } else if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode writeNode = (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = writeNode.getField();
                
                addCount(field);
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
                    case OP_getfield:
                    case OP_putfield:
                    case OP_getstatic:
                    case OP_putstatic: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            addCount(f);
                        }
                        break;
                    }
                }
            }
        }
    }

    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new FieldWriteSource(), new FieldReadTargetDatumAsField(),
            checker, this };
        ResultListener[] listeners = { checker };
        
        (new BoundedSetTracker(analyzer, specifiers, listeners, 1)).start();
    }
    
    public void printReport(Writer w) throws IOException {
        int totalFieldsAccessed = 0;
        int totalFieldsAccessedUseless = 0;
        int totalFieldAccesses = 0;
        int totalFieldUselessAccesses = 0;
        StringBuffer buf = new StringBuffer();
        
        for (Enumeration e = fieldAccessCounts.keys(); e.hasMoreElements();) {
            JBCField f = (JBCField)e.nextElement();
            int count = ((Integer)fieldAccessCounts.get(f)).intValue();
            
            totalFieldsAccessed++;
            totalFieldAccesses += count;
            
            if (!checker.isFieldUsed(f)) {
                totalFieldsAccessedUseless++;
                totalFieldUselessAccesses += count;
            
                buf.append(f).append("\n");
            }
        }
        
        w.write("Useless field accesses: "
            + GeneralBenchmark.fractionToString(totalFieldUselessAccesses, totalFieldAccesses) + "\n"
            + "Useless fields: "
            + GeneralBenchmark.fractionToString(totalFieldsAccessedUseless, totalFieldsAccessed) + "\n");
            
        w.write("Useless field list:\n" + buf);
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new UselessFieldDetector());
    }
}
