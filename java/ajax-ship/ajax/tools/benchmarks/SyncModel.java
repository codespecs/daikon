/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.Globals;
import java.io.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;
import ajax.util.*;
import java.util.*;
import ajax.tools.explain.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.jbc.typechecker.*;

public class SyncModel extends Benchmark implements ResultListener, OpcodeConstants {
    private String classNames;
    private ExplicitValuePointSource sources = new ExplicitValuePointSource();
    private ExplicitValuePointTarget targets = new ExplicitValuePointTarget();
    private String listing = null;
    private Hashtable results = new Hashtable();
    private Hashtable VPMap = new Hashtable();
    private JBCClassLoader appClassLoader = null;
    
    protected SyncModel() {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    private static String convertMethodToString(JBCMethod m) {
        return (m.isStatic() ? "static_" : "") + m.getContainingClass().getClassName()
            + "_" + m.getMethodName();
    }
    
    private static String convertNumberedVarToString(JBCMethod m, NumberedVar v) {
        String s = convertMethodToString(m);
        
        if (v instanceof NumberedVarParam) {
            return s + "_param_" + ((NumberedVarParam)v).getIndex();
        } else if (v instanceof NumberedVarComputation) {
            NumberedVarComputation c = (NumberedVarComputation)v;
            
            return s + "_" + c.getOffset() + "_" + c.getComputationNumber();
        } else {
            return "<unknownvar>";
        }
    }
    
    public void notifyAppClassLoader(JBCClassLoader loader) {
        appClassLoader = loader;
    }
    
    private static boolean isSameGotos(int PC2, int[] g1, int[] g2) {
        CompactSet set2 = new CompactSet();
        
        set2.add(new Integer(PC2));
        for (int i = 0; i < g2.length; i++) {
            set2.add(new Integer(g2[i]));
        }
        
        for (int i = 0; i < g1.length; i++) {
            if (set2.get(new Integer(g1[i])) == null) {
                return false;
            }
        }
        
        return true;
    }
    
    private void outputCodeModel(Writer w, JBCMethod m) throws IOException {
        System.err.println("Doing " + m);
        
        w.write("BEGIN " + convertMethodToString(m) + "\n");

        if (m.isAbstract()) {
            w.write("// Method is abstract\n");
        } else if (m.isNative()) {
            w.write("// Method is native\n");
        } else {
            byte[] code = m.getData().getCode();
            boolean[] starts = JBCCodeUtilities.getInstructionStarts(m.getData());
            boolean[] labelledPCs = new boolean[starts.length];
            CompactSet interestingVars = new CompactSet();
            int[][] gotos = new int[code.length][];
            BytecodeTypechecker checker = new BytecodeTypechecker(m);
            VarNumbering v = new VarNumbering(m);
            
            checker.execute();
            
            if (m.isSynchronized()) {
                NumberedVar n = v.getVariableNumber(0, 0);
                
                if (n == null) {
                    System.err.println("ERROR: Dead 'this' for synchronized method");
                } else {
                    interestingVars.add(n);
                }
            }
            
            for (int PC = 0; PC < starts.length; PC++) {
                if (starts[PC]) {
                    int[] successors = JBCCodeUtilities.getReachableSuccessors(code, PC);
                    int opcode = code[PC] & 0xFF;
                    
                    if (opcode == OP_wide) {
                        opcode = code[PC + 1] & 0xFF;
                    }
                   
                    switch (opcode) {
                        case OP_monitorexit:
                        case OP_monitorenter: {
                            NumberedVar n = v.getStackNumber(PC, 0);
                            
                            if (n != null) {
                                interestingVars.add(n);
                            }
                            break;
                        }
                        
                        case OP_invokevirtual:
                        case OP_invokestatic:
                        case OP_invokespecial:
                        case OP_invokeinterface: {
                            int paramCount = JBCCodeUtilities.getStackPopCount(m, code, PC);
                            
                            for (int i = 0; i < paramCount; i++) {
                                NumberedVar n = v.getStackNumber(PC, i);
                                
                                if (n != null) {
                                    interestingVars.add(n);
                                }
                            }
                            break;
                        }
                        
                        case OP_jsr:
                        case OP_jsr_w: {
                            int[] newSuccessors = { successors[0] };
                            
                            successors = newSuccessors;
                            break;
                        }
                        
                        case OP_ret: {
                            Vector ss = new Vector();
                            
                            for (Enumeration e = checker.getLocalVarTypes(PC,
                                JBCCodeUtilities.getLocalVariableLoadIndex(code, PC)); e.hasMoreElements();) {
                                Object o = e.nextElement();
                                
                                if (o instanceof ReturnAddrType) {
                                    ss.addElement(new Integer(((ReturnAddrType)o).getReturnAddress()));
                                }
                            }
                            
                            successors = new int[ss.size()];
                            for (int i = 0; i < successors.length; i++) {
                                successors[i] = ((Integer)ss.elementAt(i)).intValue();
                            }
                            break;
                        }
                    }
                    
                    if (successors.length == 0) {
                        int[] returner = { code.length };
                        
                        successors = returner;
                    }
                    
                    gotos[PC] = successors;
                }
            }
            
            CatchBlockData[] cblocks = m.getData().getCatchBlocks();
                    
            for (int i = 0; i < cblocks.length; i++) {
                CatchBlockData d = cblocks[i];
                
                labelledPCs[d.getHandlerPC()] = true;
                
                for (int offset = d.getStartPC(); offset < d.getEndPC(); offset++) {
                    if (starts[offset]) {
                        int[] gs2 = new int[gotos[offset].length + 1];
                            
                        System.arraycopy(gotos[offset], 0, gs2, 0, gotos[offset].length);
                        gs2[gotos[offset].length] = d.getHandlerPC();
                        gotos[offset] = gs2;
                    }
                }
            }
            
            for (Enumeration e = interestingVars.elements(); e.hasMoreElements();) {
                Object o = e.nextElement();
                JBCValuePoint vp;
                String op;
                        
                if (o instanceof NumberedVarParam) {
                    NumberedVarParam p = (NumberedVarParam)o;
                    
                    vp = new JBCValuePoint(new JBCLocation(m, 0),
                        JBCExpression.makeLocalVarExpression(p.getIndex()));
                    op = "PARAM";
                } else {
                    NumberedVarComputation c = (NumberedVarComputation)o;
                    
                    int opcode = code[c.getOffset()] & 0xFF;
                    
                    if (opcode == OP_wide) {
                        opcode = code[c.getOffset() + 1] & 0xFF;
                    }
                    
                    JBCExpression expr;

                    if (opcode == OP_iinc) {
                        expr = JBCExpression.makeLocalVarExpression(
                            JBCCodeUtilities.getLocalVariableLoadIndex(code, c.getOffset()));
                    } else {
                        expr = JBCExpression.makeStackElemExpression(c.getComputationNumber());
                    }
                    vp = new JBCValuePoint(new JBCLocation(m, 
                        JBCCodeUtilities.getReachableSuccessors(code, c.getOffset())[0]), expr);
                    
                    op = "VAR";
                }
                
                String s = convertNumberedVarToString(m, (NumberedVar)o);
                
                w.write("    " + op + " " + s + "\n");
                VPMap.put(vp, s);
                sources.addPoint(vp);
                targets.addPoint(vp);
            }
                    
            if (m.isSynchronized()) {
                w.write("    ACQUIRE " + convertNumberedVarToString(m, v.getVariableNumber(0, 0))  + "\n");
            }
            
            int lastPC = -1;
            
            for (int PC = 0; PC < starts.length; PC++) {
                if (starts[PC]) {
                    boolean doneAnything = false;
                    
                    for (Enumeration e = interestingVars.elements(); e.hasMoreElements();) {
                        Object o = e.nextElement();
                            
                        if (o instanceof NumberedVarComputation) {
                            NumberedVarComputation c = (NumberedVarComputation)o;
                                
                            if (c.getOffset() == PC) {
                                doneAnything = true;
                            }
                        }
                    }

                    switch (code[PC] & 0xFF) {
                        case OP_monitorenter:
                        case OP_monitorexit:
                        case OP_invokevirtual:
                        case OP_invokestatic:
                        case OP_invokespecial:
                        case OP_invokeinterface:
                            doneAnything = true;
                            break;
                    }
                    
                    if (lastPC >= 0 && 
                        ((!doneAnything && isSameGotos(PC, gotos[lastPC], gotos[PC]))
                        || (gotos[lastPC].length == 1 && gotos[lastPC][0] == PC))) {
                        gotos[lastPC] = null;
                    }
                
                    lastPC = PC;
                }
            }
            
            for (int PC = 0; PC < starts.length; PC++) {
                if (gotos[PC] != null) {
                    for (int i = 0; i < gotos[PC].length; i++) {
                        if (gotos[PC][i] < labelledPCs.length) {
                            labelledPCs[gotos[PC][i]] = true;
                        }
                    }
                }
            }

            for (int PC = 0; PC < starts.length; PC++) {
                if (starts[PC]) {
                    if (labelledPCs[PC]) {
                        w.write(PC + " :\n");
                    }
                    
                    for (Enumeration e = interestingVars.elements(); e.hasMoreElements();) {
                        Object o = e.nextElement();
                        
                        if (o instanceof NumberedVarComputation) {
                            NumberedVarComputation c = (NumberedVarComputation)o;
                            
                            if (c.getOffset() == PC) {
                                w.write("    DEF " + convertNumberedVarToString(m, c) + "\n");
                            }
                        }
                    }
                    
                    switch (code[PC] & 0xFF) {
                        case OP_invokevirtual:
                        case OP_invokestatic:
                        case OP_invokespecial:
                        case OP_invokeinterface: {
                            StringBuffer buf = new StringBuffer();
                            int paramCount = JBCCodeUtilities.getStackPopCount(m, code, PC);
                            
                            buf.append("    CALL ");
                            buf.append(convertMethodToString(
                                JBCCodeUtilities.resolveInstructionMethod(m, code, PC)));
                            buf.append("(");
                            
                            for (int i = paramCount - 1; i >= 0; i--) {
                                if (i < paramCount - 1) {
                                    buf.append(", ");
                                }
                                
                                NumberedVar n = v.getStackNumber(PC, i);
                                
                                if (n != null) {
                                    buf.append(convertNumberedVarToString(m, n));
                                } else {
                                    buf.append("UNKNOWN");
                                }
                            }
                            
                            buf.append(")\n");
                            w.write(buf.toString());
                            break;
                        }
                        
                        case OP_monitorexit: {
                            NumberedVar n = v.getStackNumber(PC, 0);
                            
                            if (n != null) {
                                w.write("    RELEASE " + convertNumberedVarToString(m, n) + "\n");
                            } else {
                                w.write("    RELEASE UNKNOWN\n");
                            }
                            break;
                        }
                            
                        case OP_monitorenter: {
                            NumberedVar n = v.getStackNumber(PC, 0);
                            
                            if (n != null) {
                                w.write("    ACQUIRE " + convertNumberedVarToString(m, n) + "\n");
                            } else {
                                w.write("    ACQUIRE UNKNOWN\n");
                            }
                            break;
                        }
                    }
                    
                    if (gotos[PC] != null) {
                        w.write("    GOTO");
                        for (int i = 0; i < gotos[PC].length; i++) {
                            w.write(" " + gotos[PC][i]);
                        }
                        w.write("\n");
                    }
                }
            }
            
            w.write(code.length + " :\n");
            
            if (m.isSynchronized()) {
                w.write("    RELEASE " + convertNumberedVarToString(m, v.getVariableNumber(0, 0)) + "\n");
            }
        }
        
        w.write("END " + convertMethodToString(m) + "\n\n\n");
    }
    
    public void configure(Analyzer analyzer) {
        StringWriter sw = new StringWriter();
        String[] names = StringUtils.split(classNames, ',');
            
        for (int i = 0; i < names.length; i++) {
            JBCClass c = appClassLoader.getClass(names[i]);
            
            if (c == null) {
                System.err.println("Cannot find class " + names[i]);
                System.exit(2);
                return;
            }
            
            for (Enumeration e = c.getInheritedMethods(); e.hasMoreElements();) {
                try {
                    outputCodeModel(sw, (JBCMethod)e.nextElement());
                } catch (IOException ex) {
                }
            }
        }
            
        listing = sw.toString();
            
        DatumSpecifier[] specifiers = { sources, targets };
        ResultListener[] listeners = { this };
            
        (new UnboundedSetTracker(analyzer, specifiers, listeners)).start();
    }
    
    public void printReport(Writer w) throws IOException {
        w.write(listing);
        w.write("BEGIN ALIASES\n");
        
        for (Enumeration e = VPMap.keys(); e.hasMoreElements();) {
            JBCValuePoint vp = (JBCValuePoint)e.nextElement();
            
            w.write(VPMap.get(vp) + " :");
            for (Enumeration e2 = UnboundedSetTracker.enumerateIntermediate(results.get(vp));
                e2.hasMoreElements();) {
                w.write(" " + VPMap.get(e2.nextElement()));
            }
            w.write("\n");
        }
        
        w.write("END ALIASES\n");
    }
    
    public void registerTarget(Object targetCookie) {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate == null) {
            results.remove(targetCookie);
        } else {
            results.put(targetCookie, intermediate);
        }
    }
    
    public void parseArgs(Args args) {
        classNames = args.extractNextArg("<classnames>");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new SyncModel(),
            "<classnames>");
    }
}
