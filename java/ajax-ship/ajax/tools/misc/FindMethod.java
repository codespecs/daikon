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

public class FindMethod implements OpcodeConstants {
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings, "Usage: <classes-glob> <method-glob> [<classpath>]");
        String classesGlob = args.extractNextArg("<classes-glob>");
        String methodGlob = args.extractNextArg("<method-glob>");
        String classPath = args.extractNextOptionalArg();
        
        args.checkDone();
        
        JBCStatsConfig cfg = new JBCStatsConfig(classPath);
        
        cfg.init();
        
        StandardClassLoader loader = cfg.getLoader();
        GlobMatcher mGlob = new GlobMatcher(methodGlob);

        CompactSet classes = GlobClassMask.makeFrom(classesGlob).findMatchingClasses(cfg.getLoader());
                
        for (Enumeration e = classes.elements(); e.hasMoreElements();) {
            JBCClass c = loader.getClass((String)e.nextElement());
            boolean found = false;
                
            for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e2.nextElement();
                    
                if (mGlob.isMatch(m.getMethodName())) {
                    found = true;
                }
            }
                
            if (found) {
                System.out.println(c.getClassName());
            }
        }
    }
}
