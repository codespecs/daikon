/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.misc;

import java.io.*;
import java.util.*;

public class DumpVMInfo {
    private static final int MB = 1024*1024;
    
    private static String memString(long bytes) {
        return Long.toString((bytes + MB - 1)/MB) + "MB";
    }
    
    private static final String classNames[] = {
        "ajax.util.CompactSet",
        "java.util.Hashtable",
        "java.util.Hashtable$Entry"
    };
    
    private static final int objectCount = 100000;
    
    private static void printStandardClassName(Writer w, String name, Object o) throws IOException {
        w.write("Class name for " + name + " is: " + o.getClass().getName() + "\n");
    }
    
    private static void printStandardClassNames(Writer w) throws IOException {
        printStandardClassName(w, "Object", new Object());
        printStandardClassName(w, "String", new String());
        printStandardClassName(w, "DumpVMInfo", new DumpVMInfo());
    }
    
    private static void printObjectSize(Writer w, String name) throws IOException {
        Object[] refs = new Object[objectCount];
        
        try {
            Class c = Class.forName(name);
            
            System.gc();
            
            long initialFreeMem = Runtime.getRuntime().freeMemory();
            
            for (int i = 0; i < refs.length; i++) {
                refs[i] = c.newInstance();
            }
            
            System.gc();
            
            float size = (initialFreeMem - Runtime.getRuntime().freeMemory())/objectCount;
            
            w.write("Size of " + name + " objects: " + size + "\n");
        } catch (ClassNotFoundException ex) {
            w.write("Class " + name + " not found\n");
        } catch (InstantiationException ex) {
            w.write("Class " + name + " could not be instantiated\n");
        } catch (IllegalAccessException ex) {
            w.write("Class " + name + " is inaccessible\n");
        }
    }
    
    private static void printMemoryStats(Writer w) throws IOException {
        w.write("Memory statistics:\n");
        w.write("    Total memory = " + memString(Runtime.getRuntime().totalMemory()) + "\n");
        w.write("    Free memory = " + memString(Runtime.getRuntime().freeMemory()) + "\n");
        
        for (int i = 0; i < classNames.length; i++) {
            printObjectSize(w, classNames[i]);
        }
    }
    
    private static void printSystemProperties(Writer w) throws IOException {
        w.write("System properties:\n");
        
        Properties p = System.getProperties();
        
        for (Enumeration e = p.keys(); e.hasMoreElements();) {
            String key = (String)e.nextElement();
            
            w.write("    " + key + "=" + p.get(key) + "\n");
        }
    }
    
    public static void printReport(Writer w) throws IOException {
        printMemoryStats(w);
        printSystemProperties(w);
        printStandardClassNames(w);
    }
    
    public static void main(String[] args) {
        try {
            Writer w = new OutputStreamWriter(System.out);
           
            printReport(w);
            w.flush();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
