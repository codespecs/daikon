/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.jbc.typechecker.*;
import java.util.*;

public class Test1 {
    public static String toString(Object t) {
        if (t == JBCType.OBJECT) {
            return "null";
        } else {
            return t.toString();
        }
    }
    
    public static void main(String[] args) {
        if (args.length < 2) {
            System.err.println("Usage: <classname> <methodname>");
            return;
        }
        
        JBCWorld world = new JBCWorld();
        StandardClassLoader loader = new StandardClassLoader(world);
        
        world.setSystemClassLoader(loader);
        
        JBCClass c = loader.getClass(args[0]);
        
        if (c == null) {
            System.err.println("Cannot load class " + args[0]);
            return;
        }
        
        int numFound = 0;

        for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            
            if (m.getMethodName().equals(args[1])) {
                System.out.println("Types in " + m);
                
                BytecodeTypechecker checker = new BytecodeTypechecker(m);
                
                checker.setEnablePreciseClasses(true);
                checker.setTrackInstanceOf(true);
                checker.execute();
                
                boolean[] starts = JBCCodeUtilities.getInstructionStarts(m.getData());
                
                for (int PC = 0; PC < starts.length; PC++) {
                    if (starts[PC]) {
                        System.out.println("    PC = " + PC + ":");
                        for (int i = 0; i < m.getData().getMaxStackWords(); i++) {
                            System.out.println("        Stack elem " + i + ":");
                            for (Enumeration e2 = checker.getStackElemTypes(PC, i); e2.hasMoreElements();) {
                                System.out.println("            " + toString(e2.nextElement()));
                            }
                        }
                        for (int i = 0; i < m.getData().getMaxLocalWords(); i++) {
                            System.out.println("        Local var " + i + ":");
                            for (Enumeration e2 = checker.getLocalVarTypes(PC, i); e2.hasMoreElements();) {
                                System.out.println("            " + toString(e2.nextElement()));
                            }
                        }
                    }
                }
                
                numFound++;
            }
        }
        
        if (numFound == 0) {
            System.err.println("No method found: " + args[1]);
        }
    }
    
    public static int jsrEvil() {
        int i = 0;
        
        try {
            while (i < 10) {
                try {
                    if (i == 7) {
                        return 0;
                    }
                } finally {
                    if (i >= 5) {
                        continue;
                    }
                }
            }
        } finally {
            i++;
        }
        
        return i;
    }
}
