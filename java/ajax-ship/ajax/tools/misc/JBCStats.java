/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import ajax.util.*;
import ajax.jbc.*;
import ajax.jbc.util.*;
import java.io.*;
import java.util.*;

public class JBCStats {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings, "Usage: <glob> [<classpath>]");
        String glob = args.extractNextArg("glob");
        String classPath = args.extractNextOptionalArg();
        
        args.checkDone();
        
        JBCStatsConfig cfg = new JBCStatsConfig(classPath);
        
        cfg.init();
        
        CompactSet classes = GlobClassMask.makeFrom(glob).findMatchingClasses(cfg.getLoader());
        int numMethods = 0;
        int bytecodeBytes = 0;
                
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            JBCClass c = cfg.getLoader().getClass((String)e.nextElement());
                
            for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e2.nextElement();
                byte[] code = m.getData().getCode();
                    
                if (code != null) {
                    if (!m.getMethodName().equals("<clinit>")) {
                        bytecodeBytes += code.length;
                    }
                    numMethods++;
                }
            }
        }
            
        System.out.println("Number of classes: " + classes.size());
        System.out.println("Number of method bodies: " + numMethods);
        System.out.println("Bytecode bytes: " + bytecodeBytes);
    }
}
