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

public class MakeNativeMethodStubs implements OpcodeConstants {
    private static void churn(InputStream from, OutputStream to) throws IOException {
        byte[] buf = new byte[16384];
        
        while (true) {
            int amountRead = from.read(buf);
            
            if (amountRead < 0) {
                return;
            }
            
            to.write(buf, 0, amountRead);
        }
    }
    
    private static void outputBytecodeDisassembly(JBCClass c) {
        String className = c.getClassName();
        int lastDot = className.lastIndexOf('.');
        String lastName = lastDot > 0 ? className.substring(lastDot + 1) : className;
        File outDir = new File("decompiled");
        File outName = new File(outDir, lastName + ".p");
        
        if (!outName.exists()) {
            try {
                String[] params = { "javap", "-c", "-private", className };
                Process p = Runtime.getRuntime().exec(params);
                FileOutputStream out = new FileOutputStream(outName);
                
                churn(p.getInputStream(), out);
                out.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }
    
    private static void outputNativeMethodStub(Writer w, JBCMethod m, boolean firstMethod) throws IOException {
        String className = m.getContainingClass().getClassName();
        StringBuffer params = new StringBuffer();
        
        if (firstMethod) {
            w.write("/* " + className + " */\n\n");
        }
        
        JBCMethodType methodType = m.getMethodType();
        JBCType[] paramTypes = methodType.getParameterTypes();
        
        for (int i = 0; i < paramTypes.length; i++) {
            String paramName = !m.isStatic()
                ? (i == 0 ? "THIS" : "P" + (i - 1) + " /* " + paramTypes[i].toString() + " */")
                : "P" + i + " /* " + paramTypes[i].toString() + " */";
            
            if (i > 0) {
                params.append(", ");
            }
            params.append(paramName);
        }
        
        w.write(className + "." + m.getMethodName() + "(" + params.toString() + ") {\n");
        
        JBCType resultType = methodType.getReturnType();
        
        if (!resultType.equals(JBCType.VOID)) {
            if (resultType instanceof JBCObjectType) {
                w.write("    return = something; /* " + resultType.toString() + " */\n");
            } else {
                w.write("    return = choose; /* " + resultType.toString() + " */\n");
            }
        }
        
        w.write("}\n\n");
    }
    
    public static void main(String[] argStrings) {
        Args args = new Args(argStrings, "Usage: <classes-glob> [-cp <classpath>]");
        String classesGlob = args.extractNextArg("<classes-glob>");
        String classPath = args.extractStringOption("-cp", null);
        
        args.checkDone();
        
        JBCStatsConfig cfg = new JBCStatsConfig(classPath);
        
        cfg.init();
        
        StandardClassLoader loader = cfg.getLoader();

        try {
            CompactSet classes = GlobClassMask.makeFrom(classesGlob).findMatchingClasses(loader);
            Writer w = new BufferedWriter(new OutputStreamWriter(System.out));
                
            for (Enumeration e = classes.elements(); e.hasMoreElements();) {
                JBCClass c = loader.getClass((String)e.nextElement());
                boolean firstMethod = true;
                MethodData[] methods = c.getData().getMethods();
                
                for (int i = 0; i < methods.length; i++) {
                    JBCMethod m = c.getMethod(methods[i].getMethodName(),
                        methods[i].getMethodType());
                    
                    if (m.isNative()) {
                        outputNativeMethodStub(w, m, firstMethod);
                        firstMethod = false;
                    }
                }
                
                if (!firstMethod) {
                    outputBytecodeDisassembly(c);
                }
            }
            
            w.flush();
        } catch (IOException ex) {
            System.err.println("Error scanning classes: " + ex);
        }
    }
}
