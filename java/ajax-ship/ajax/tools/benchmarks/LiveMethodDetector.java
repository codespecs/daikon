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

public class LiveMethodDetector extends Benchmark implements ResultListener, DatumSpecifier, OpcodeConstants {
    private Hashtable queryLiveMethods = new Hashtable();
    private Hashtable staticLiveMethods = new Hashtable();
    private int setBounds;
    private Object unknownLocation = "<unknown>";
    private JBCClassLoader appClassLoader = null;
    
    protected LiveMethodDetector() {
    }

    public void registerTarget(Object targetCookie) {
    }
    
    public void notifyAnalysisComplete() {
    }
    
    public void updateResult(Object targetCookie, Object intermediate) {
        if (intermediate != null) {
            queryLiveMethods.put((JBCMethod)targetCookie, intermediate);
        } else {
            queryLiveMethods.remove((JBCMethod)targetCookie);
        }
    }
    
    public static String getIntermediateString(Object intermediate) {
        if (intermediate instanceof OverflowSet || intermediate instanceof CompactSet) {
            return BoundedSetTracker.toString(intermediate);
        } else {
            return intermediate.toString();
        }
    }
    
    void addClassInstantiation(Object location, JBCClass c) {
        JBCMethod finalizerMethod = c.getInheritedMethod("finalize", "()V");
        
        if (finalizerMethod != null) {
            staticLiveMethods.put(finalizerMethod, unknownLocation);
        }
        
        addClassInitialization(location, c);
    }
    
    void addClassInitialization(Object location, JBCClass c) {
        JBCMethod initializer = c.getInitializerMethod();
        
        if (initializer != null) {
            staticLiveMethods.put(initializer, location);
        }
        
        JBCClass superClass = c.getSuperClass();
        
        if (superClass != null) {
            addClassInitialization(location, superClass);
        }
    }
    
    private static Object makeLocation(Object site, ExternalCFGNode node) {
        if (site instanceof String) {
            return new ExternalLocation((String)site, node);
        } else {
            return new ExternalLocation((JBCMethod)site, node);
        }
    }
    
    private void identifyQueryData(Object site, Vector externalNodes) {
        for (Enumeration e = externalNodes.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o instanceof ExternalCFGMethodInvocationDefNode) {
                ExternalCFGMethodInvocationDefNode node = (ExternalCFGMethodInvocationDefNode)o;
                JBCMethod m = node.getMethod(); 
               
                if (JBCCodeUtilities.useStaticDispatch(m)) {
                    staticLiveMethods.put(m, makeLocation(site, node));
                } else if (m.isStatic()) {
                    addClassInitialization(makeLocation(site, node), m.getContainingClass());
                }
            } else if (o instanceof ExternalCFGNewObjectDefNode) {
                ExternalCFGNewObjectDefNode newNode = (ExternalCFGNewObjectDefNode)o;
                
                addClassInstantiation(makeLocation(site, newNode), newNode.getObjectClass());
            } else if (o instanceof ExternalCFGFieldAssignmentDefNode) {
                ExternalCFGFieldAssignmentDefNode assignDefNode =
                    (ExternalCFGFieldAssignmentDefNode)o;
                JBCField field = assignDefNode.getField();

                if (field.isStatic()) {
                    addClassInitialization(makeLocation(site, assignDefNode), field.getContainingClass());
                }
            } else if (o instanceof ExternalCFGFieldDefNode) {
                ExternalCFGFieldDefNode fieldNode = (ExternalCFGFieldDefNode)o;
                JBCField field = fieldNode.getField();

                if (field.isStatic()) {
                    addClassInitialization(makeLocation(site, fieldNode), field.getContainingClass());
                }
            }
        }
    }
        
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(method, externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, String name, ExternalFlowgraph fg, Vector externalNodes) {
        identifyQueryData(name, externalNodes);
    }
    
    public void identifyQueryData(IndirectionQueryFamily family, JBCMethod method, byte[] code, boolean[] instructions) {
        for (int i = 0; i < code.length; i++) {
            if (instructions[i]) {
                switch (code[i] & 0xFF) {
                    case OP_new:
                    case OP_newarray:
                    case OP_anewarray:
                    case OP_multianewarray: {
                        JBCClass c = JBCCodeUtilities.resolveInstructionClass(method, code, i);
                        
                        if (c != null) {
                            addClassInstantiation(new JBCLocation(method, i), c);
                        }
                        break;
                    }
                    
                    case OP_getstatic:
                    case OP_putstatic: {
                        JBCField f = JBCCodeUtilities.resolveInstructionField(method, code, i);
                        
                        if (f != null) {
                            addClassInitialization(new JBCLocation(method, i),                     
                                f.getContainingClass());
                        }
                        break;
                    }
                    
                    case OP_invokestatic: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (m != null) {
                            JBCLocation loc = new JBCLocation(method, i);
                            
                            staticLiveMethods.put(m, loc);
                            addClassInitialization(loc, m.getContainingClass());
                        }
                        break;
                    }
                    
                    case OP_invokespecial: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);
                        
                        if (m != null) {
                            staticLiveMethods.put(m, new JBCLocation(method, i));
                        }
                        break;
                    }
                    
                    case OP_invokevirtual:
                    case OP_invokeinterface: {
                        JBCMethod m = JBCCodeUtilities.resolveInstructionMethod(method, code, i);

                        if (m != null && JBCCodeUtilities.useStaticDispatch(m)) {
                            staticLiveMethods.put(m, new JBCLocation(method, i));
                        }
                        break;
                    }
                }
            }
        }
    }
    
    public void configure(Analyzer analyzer) {
        DatumSpecifier[] specifiers = { new NewObjectMethodTarget(), new VirtualCallReceiverSource(), this };
        ResultListener[] listeners = { this };
        
        (new BoundedSetTracker(analyzer, specifiers, listeners, setBounds)).start();
        
        staticLiveMethods.put(
            analyzer.getWorld().getSpecialClass("java.lang.System").getMethod("gc", "()V"),
            unknownLocation);
    }
    
    private static void addTable(Hashtable dest, Hashtable src) {
        for (Enumeration e = src.keys(); e.hasMoreElements();) {
            Object key = e.nextElement();
            Object value = src.get(key);
            Object curValue = dest.get(key);
            
            if (curValue == null) {
                dest.put(key, value);
            } else {
                dest.put(key, BoundedSetTracker.join(value, curValue, 1));
            }
        }
    }
    
    protected void notifyAppClassLoader(JBCClassLoader appClassLoader) {
        this.appClassLoader = appClassLoader;
    }
    
    public void printReport(Writer w) throws IOException {
        int methodCount = 0;
        int bytecodeBytes = 0;
        int deadMethods = 0;
        int deadBytes = 0;
        int totalMethods = 0;
        Hashtable liveMethods = new Hashtable();
        
        addTable(liveMethods, queryLiveMethods);
        addTable(liveMethods, staticLiveMethods);
        
        for (Enumeration e = liveMethods.keys(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            byte[] code = m.getData().getCode();
            
            if ((appClassLoader == null || m.getContainingClass().getClassLoader().equals(appClassLoader))
                && !m.getMethodName().equals("<clinit>") && code != null) {
                Object value = liveMethods.get(m);
                int codeBytes = code.length;
                
                w.write("Callers for live " + m + ": " + getIntermediateString(value)
                    + " (" + codeBytes + " bytes)\n");
                    
                bytecodeBytes += codeBytes;
                methodCount++;
            }
        }
        
        w.write("Number of live methods: " + methodCount + "\n");
        w.write("Live bytecode bytes: " + bytecodeBytes + "\n");
        
        CompactSet visitedClasses = new CompactSet();
        
        if (appClassLoader instanceof StandardClassLoader) {
            for (Enumeration e = ((StandardClassLoader)appClassLoader).getClassList(); e.hasMoreElements();) {
                String className = (String)e.nextElement();
                JBCClass c = appClassLoader.getClass(className);
                
                if (c != null && visitedClasses.get(c) == null) {
                    visitedClasses.addUnconditionally(c);
                    
                    for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                        JBCMethod m = (JBCMethod)e2.nextElement();
                        byte[] code = m.getData().getCode();
                        
                        if (!m.getMethodName().equals("<clinit>") && code != null) {
                            if (liveMethods.get(m) == null) {
                                deadMethods++;
                                deadBytes += code.length;
                                w.write("Dead method: " + m + "\n");
                            }
                            
                            totalMethods++;
                        }
                    }
                }
            }
        }
        
        w.write("Number of dead methods: " + deadMethods + "\n");
        w.write("Dead bytecode bytes: " + deadBytes + "\n");
        if (totalMethods != methodCount + deadMethods) {
            w.write("Warning: total methods mismatch, equals " + totalMethods + "\n");
        }
    }
    
    public void parseOptions(Args args) {
        setBounds = args.extractIntOption("-set", 1);
    }
    
    public static void main(String[] args) {
        GeneralBenchmark.run(args, new LiveMethodDetector(), "[-set <N>]");
    }
}
