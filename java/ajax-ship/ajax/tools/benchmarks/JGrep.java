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

public class JGrep extends Benchmark implements ResultListener, OpcodeConstants {
    private String expr;
    private boolean readMode;
    private boolean writeMode;
    private boolean callMode;
    private boolean newMode;
    private CompactSet printedLocations = new CompactSet();
    private JBCClassLoader appLoader;
    
    protected JGrep() {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
        appLoader = appClassLoader;
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier sources;
        
        try {
            sources = ExplicitValuePointSource.makeFromQuery(appLoader, expr);
        } catch (ParseException ex) {
            System.err.println("Error in expression " + expr + ": "
                + ex.getMessage());
            System.exit(2);
            return;
        }
        
        Vector targets = new Vector();
        
        if (readMode) {
            targets.addElement(new AnyFieldReadTarget());
        }
        if (writeMode) {
            targets.addElement(new AnyFieldWriteTarget());
        }
        if (newMode) {
            targets.addElement(new NewObjectTarget());
        }
        if (callMode) {
            targets.addElement(new AnyMethodInvocationOnThisTarget());
        }
        
        DatumSpecifier[] specifiers = new DatumSpecifier[targets.size() + 1];
        
        targets.copyInto(specifiers);
        specifiers[specifiers.length - 1] = sources;
        
        ResultListener[] listeners = { this };
        
        (new SingletonTracker(analyzer, specifiers, listeners)).start();
    }
    
    public void printReport(Writer w) throws IOException {
    }
    
    public void registerTarget(Object targetCookie) {
    }
    
    private static String getOperationName(Location l) {
        if (l instanceof ExternalLocation) {
            ExternalCFGNode node = ((ExternalLocation)l).getNode();
            
            if (node instanceof ExternalCFGFieldDefNode) {
                return "READ from " + ((ExternalCFGFieldDefNode)node).getField();
            } else if (node instanceof ExternalCFGFieldAssignmentDefNode) {
                return "WRITE to " + ((ExternalCFGFieldAssignmentDefNode)node).getField();
            } else if (node instanceof ExternalCFGNewObjectDefNode) {
                return "NEW of " + ((ExternalCFGNewObjectDefNode)node).getObjectClass();
            } else if (node instanceof ExternalCFGMethodInvocationDefNode) {
                return "CALL to " + ((ExternalCFGMethodInvocationDefNode)node).getMethod();
            }
        } else if (l instanceof JBCLocation) {
            JBCLocation j = (JBCLocation)l;
            JBCMethod method = j.getMethod();
            byte[] code = method.getData().getCode();
            int offset = j.getOffset();
            
            switch (code[offset] & 0xFF) {
                case OP_invokevirtual:
                case OP_invokespecial:
                case OP_invokeinterface:
                    return "CALL to " + JBCCodeUtilities.resolveInstructionMethod(method, code, offset);
                    
                case OP_putfield:
                    return "WRITE to " + JBCCodeUtilities.resolveInstructionField(method, code, offset);
                    
                case OP_aastore:
                case OP_fastore:
                case OP_iastore:
                case OP_sastore:
                case OP_castore:
                case OP_bastore:
                case OP_dastore:
                case OP_lastore:
                    return "WRITE to array contents";

                case OP_aaload:
                case OP_faload:
                case OP_daload:
                case OP_laload:
                case OP_iaload:
                case OP_saload:
                case OP_caload:
                case OP_baload:
                    return "READ from array contents";
                    
                case OP_arraylength:
                    return "READ from array length";
                    
                case OP_getfield:
                    return "READ from " + JBCCodeUtilities.resolveInstructionField(method, code, offset);
                    
                case OP_newarray:
                case OP_anewarray:
                case OP_new:
                case OP_multianewarray:
                    return "NEW of " + JBCCodeUtilities.resolveInstructionClass(method, code, offset);
            }
        }
        
        Globals.localError("Uknown field type at " + l);
        return "UNKNOWN: ";
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        Location l = (Location)targetCookie;
        
        if (intermediate != null && printedLocations.get(l) == null) {
            printedLocations.addUnconditionally(l);
            
            System.out.println(getOperationName(l) + ":\n    " + l.toString());
        }
    }
    
    public void parseArgs(Args args) {
        readMode = args.extractBoolOption("-read");
        writeMode = args.extractBoolOption("-write");
        newMode = args.extractBoolOption("-new");
        callMode = args.extractBoolOption("-calls");
        
        if (!readMode && !writeMode && !newMode && !callMode) {
            readMode = true;
            writeMode = true;
            newMode = true;
            callMode = true;
        }
        
        expr = args.extractNextArg("<value-point>");
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new JGrep(),
            "<value-point> [-read] [-write] [-new] [-calls]");
    }
}
