/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import java.io.*;

public class Test1 implements OpcodeConstants {
    private static boolean checkName(String list, String name) {
        while (list != null) {
            int lastComma = list.indexOf(",");
            String prefix;
            
            if (lastComma < 0) {
                prefix = list;
                list = null;
            } else {
                prefix = list.substring(0, lastComma);
                list = list.substring(lastComma + 1);
            }
            
            if (name.startsWith(prefix)) {
                return true;
            }
        }
        
        return false;
    }
    
    public static void main(String[] args) {
        JBCWorld world = new JBCWorld();
        StandardClassLoader parent = new StandardClassLoader(world,
            "d:\\programs\\jdk117\\lib\\classes.zip");
        StandardClassLoader loader = new StandardClassLoader(world, args[0]);
        int bytecodeBytes = 0;
        int bytecodeMethods = 0;
        int classes = 0;
        
        loader.setParentClassLoader(parent);
        
        for (Enumeration e = loader.getClassList(); e.hasMoreElements();) {
            String className = (String)e.nextElement();
            ClassData c = loader.loadClassData(className);
            
            if (c != null && (args.length < 2 || checkName(args[1], c.getClassName()))) {
                System.out.println(c.getClassName());
                
                classes++;
                
                MethodData[] methods = c.getMethods();

                for (int i = 0; i < methods.length; i++) {
                    MethodData m = methods[i];
                    
                    if (!m.getMethodName().equals("<clinit>")) {
                        try {
                            byte[] code = m.getCode();
     
                            if (code != null) {
                                bytecodeMethods++;
                                bytecodeBytes += code.length;
                            }
                        } catch (InvalidClassDataError ex) {
                            System.out.println("Error in bytecode for method " + m.getMethodName() + ": " + ex.getMessage());
                        }
                    }
                }
            }
        }
        
        System.out.println("Classes: " + classes);
        System.out.println("Bytecode methods: " + bytecodeMethods);
        System.out.println("Bytecode bytes: " + bytecodeBytes);
    }
}
