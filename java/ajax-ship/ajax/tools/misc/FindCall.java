/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import ajax.util.*;
import ajax.jbc.util.reflect.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;
import ajax.analyzer.util.*;

public class FindCall implements OpcodeConstants {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings, "Usage: <glob> <function> [<classpath>] [-omitcaller <name>]");
        String glob = args.extractNextArg("<glob>");
        String fun = args.extractNextArg("<function>");
        String classPath = args.extractNextOptionalArg();
        String omitCaller = args.extractStringOption("-omitcaller", null);
        
        args.checkDone();
        
        JBCStatsConfig cfg = new JBCStatsConfig(classPath);
        
        cfg.init();
        
        StandardClassLoader loader = cfg.getLoader();
        CompactSet methods;
        
        try {
            methods = JBCParserUtilities.parseFunctionSet(loader, fun);
        } catch (UnresolvedClassException ex) {
            System.err.println(ex.getMessage());
            return;
        } catch (MissingMethodException ex) {
            System.err.println(ex.getMessage());
            return;
        } catch (AmbiguousMethodException ex) {
            System.err.println(ex.getMessage());
            return;
        }
        
        CompactSet classes = GlobClassMask.makeFrom(glob).findMatchingClasses(cfg.getLoader());
                
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            JBCClass c = loader.getClass((String)e.nextElement());
            boolean found = false;
                
            for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e2.nextElement();
                    
                if (omitCaller == null || !omitCaller.equals(m.getMethodName())) {
                    MethodData mData = m.getData();
                    byte[] code = mData.getCode();
                        
                    if (code != null) {
                        boolean[] instructionStarts = JBCCodeUtilities.getInstructionStarts(mData);
                            
                        for (int i = 0; i < instructionStarts.length; i++) {
                            if (instructionStarts[i]) {
                                switch (code[i] & 0xFF) {
                                    case OP_invokevirtual:
                                    case OP_invokespecial:
                                    case OP_invokestatic:
                                    case OP_invokeinterface: {
                                        JBCMethod callee = JBCCodeUtilities.resolveInstructionMethod(m, code, i);
                                            
                                        if (callee != null && methods.get(callee) != null) {
                                            found = true;
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
                
            if (found) {
                System.out.println(c.getClassName());
            }
        }
    }
}
