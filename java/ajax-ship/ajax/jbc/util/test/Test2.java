/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.test;

import ajax.jbc.*;
import ajax.jbc.util.*;
import java.util.*;
import java.io.*;

public class Test2 implements OpcodeConstants {
    private static long bytecodeBytes = 0;
    private static long bytecodeBytesWithSource = 0;
    private static long sourceFileLines = 0;
    private static long bytecodeMethods = 0;
    private static long bytecodeMethodsWithJSR = 0;
    
    private static int f() {
        int x = 0;
        while (true) {
            try {
                x++;
                if (x == 0) return x;
            } finally {
                if (x > 0) continue;
            }
        }
    }
    
    private static void g() {
        try {
            try {
                System.out.println("HO");
            } finally {
                System.out.println("HO");
            }
        } finally {
            System.out.println("HO");
        }
    }
    
    private static boolean markSubs(byte[] code, int[] subs, CatchBlockData[] catches, int offset, int sub, int[] one) {
        while (true) {
            try {
                if (subs[offset] == sub) {
                    return false;
                } else if (subs[offset] != 0) {
                    return true;
                }
                
                subs[offset] = sub;
                
                for (int i = 0; i < catches.length; i++) {
                    CatchBlockData cblock = catches[i];
                    
                    if (offset >= cblock.getStartPC() && offset < cblock.getEndPC()) {
                        if (markSubs(code, subs, catches, cblock.getHandlerPC(), sub, one)) {
                            return true;
                        }
                    }
                }
                
                int opcode = code[offset] & 0xFF;
                int[] next = JBCCodeUtilities.getReachableSuccessors(code, offset, one);
                
                if (opcode == OP_jsr || opcode == OP_jsr_w) {
                    offset = next[0];
                    
                    if (markSubs(code, subs, catches, next[1], next[1], one)) {
                        return true;
                    }
                } else if (next.length == 0) {
                    return false;
                } else {
                    offset = next[0];
                    
                    for (int i = 1; i < next.length; i++) {
                        if (markSubs(code, subs, catches, next[i], sub, one)) {
                            return true;
                        }
                    }
                }
            } catch (InvalidClassDataError ex) {
                System.out.println("Error at offset: " + offset);
                throw ex;
            }
        }
    }
    
    private static boolean checkMethod(MethodData m) {
        byte[] code = m.getCode();
        
        if (code != null) {
            int[] subs = new int[code.length];
            CatchBlockData[] catches = m.getCatchBlocks();
            boolean result = markSubs(code, subs, catches, 0, -1, new int[1]);
            
            for (int i = 0; i < subs.length; i++) {
                if (subs[i] > 0) {
                    bytecodeMethodsWithJSR++;
                    break;
                }
            }
            bytecodeMethods++;
            
            return result;
        } else {
            return false;
        }
    }
    
    private static final String[] sourcePath = {
        "d:\\programs\\jdk117\\src",
        "c:\\users\\roc\\lackwit\\ajax",
        "c:\\users\\roc\\lackwit\\ajax\\examples\\ctas",
        "c:\\users\\roc\\lackwit\\ajax\\examples\\jess44",
        "c:\\users\\roc\\lackwit\\ajax\\examples\\java2html",
    };
    
    private static int countSourceLines(File f) throws IOException {
        StreamTokenizer t = new StreamTokenizer(new BufferedReader(new FileReader(f)));
        int lines = 0;
        int token;
        boolean seenCh = false;
        
        t.resetSyntax();
        t.whitespaceChars(0, 32);
        t.slashSlashComments(true);
        t.slashStarComments(true);
        t.eolIsSignificant(true);
        t.quoteChar('\'');
        t.quoteChar('"');
        
        while ((token = t.nextToken()) != StreamTokenizer.TT_EOF) {
            if (token == StreamTokenizer.TT_EOL) {
                if (seenCh) {
                    seenCh = false;
                    lines++;
                }
            } else {
                seenCh = true;
            }
        }
        
        if (seenCh) {
            lines++;
        }
        
        return lines;
    }
    
    private static String getSourceFileName(ClassData c) {
        String className = c.getClassName();
        int lastDot = className.lastIndexOf('.');
        String sourceFileName = c.getSourceFileName();
        
        if (sourceFileName == null) {
            sourceFileName = className.substring(lastDot + 1) + ".java";
        }

        return className.substring(0, lastDot + 1).replace('.', File.separatorChar) + sourceFileName;
    }
    
    private static boolean checkSourceFile(String sourceName) {
        for (int i = 0; i < sourcePath.length; i++) {
            File f = new File(sourcePath[i], sourceName);
            
            if (f.exists()) {
                try {
                    sourceFileLines += countSourceLines(f);
                    return true;
                } catch (IOException ex) {
                    System.out.println("Error reading file " + f + ": " + ex.getMessage());
                    return false;
                }
            }
        }
        
        return false;
    }
    
    private static boolean runTest() {
        JBCWorld world = new JBCWorld();
        StandardClassLoader loader = new StandardClassLoader(world,
            ".;d:\\programs\\jdk117\\lib\\classes.zip;" +
            "examples\\ctas;examples\\jess44;examples\\java2html");
        Hashtable seenClasses = new Hashtable();
        Hashtable seenSourceFiles = new Hashtable();
        
        for (Enumeration e = loader.getClassList(); e.hasMoreElements();) {
            String className = (String)e.nextElement();
            ClassData c = loader.loadClassData(className);
            
            if (c != null && !seenClasses.containsKey(className)) {
                MethodData[] methods = c.getMethods();
                String sourceFileName = getSourceFileName(c);
                boolean seenSource = seenSourceFiles.containsKey(sourceFileName);
                boolean haveSource = !seenSource && checkSourceFile(sourceFileName);
                
                seenClasses.put(className, className);
                
                if (haveSource) {
                    seenSourceFiles.put(sourceFileName, sourceFileName);
                }

                for (int i = 0; i < methods.length; i++) {
                    MethodData m = methods[i];
                    
                    try {
                        byte[] code = m.getCode();
 
                        if (code != null) {
                            if (haveSource) {
                                bytecodeBytesWithSource += code.length;
                            }
                            
                            bytecodeBytes += code.length;
                        
                            if (checkMethod(m)) {
                                System.out.println(c.getClassName() + ": " + m.getMethodName());
                            }
                        }
                    } catch (InvalidClassDataError ex) {
                        System.out.println("Error in bytecode for method " + m.getMethodName() + ": " + ex.getMessage());
                    }
                }
            }
        }
        
        System.out.println("Bytecode bytes: " + bytecodeBytes + ", bytes with source: " + bytecodeBytesWithSource + ", nonblank noncomment source lines: " + sourceFileLines);
        System.out.println("Methods with code: " + bytecodeMethods + ", number with at least one subroutine: " + bytecodeMethodsWithJSR);

        return true;
    }

    public static void main(String[] args) {
        System.out.print("ajax.jbc.util.test.Test2: ");

        if (runTest()) {
            System.out.println("passed");
        } else {
            System.out.println("failed");
        }
    }
}
