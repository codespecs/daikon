/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import java.util.Enumeration;
import ajax.jbc.*;
import ajax.util.*;
import ajax.analyzer.*;
import java.io.*;
import ajax.Globals;

public class BasicAnalyzerStats implements AnalyzerStats, JBCObserver {
    private int loadedBytecodeMethods = 0;
    private int liveBytecodeMethods = 0;
    private int loadedBytecodeBytes = 0;
    private int liveBytecodeBytes = 0;
    private CompactSet liveClasses = new CompactSet();
    private CompactSet liveMethods = new CompactSet();
    
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
    }
    
    public void notifyAddedMainInvocation(String name) {
    }
    
    public void notifyClassLoaded(JBCClass c) {
    }
    
    public void notifyMethodLive(JBCMethod m) {
        if (liveMethods.get(m) == null) {
            byte[] code = m.getData().getCode();
            
            liveMethods.addUnconditionally(m);
            
            if (code != null) {
                liveBytecodeMethods++;
                if (!m.getMethodName().equals("<clinit>")) {
                    liveBytecodeBytes += code.length;
                }
            }
            
            JBCClass c = m.getContainingClass();
            
            if (liveClasses.get(c) == null) {
                liveClasses.addUnconditionally(c);
                
                for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
                    JBCMethod m2 = (JBCMethod)e.nextElement();
                    byte[] code2 = m2.getData().getCode();
                    
                    if (code2 != null) {
                        loadedBytecodeMethods++;
                        if (!m2.getMethodName().equals("<clinit>")) {
                            loadedBytecodeBytes += code2.length;
                        }
                    }
                }
            }
        } else if (Globals.debug) {
            Globals.nonlocalError("Method live again: " + m);
        }
    }
    
    public void notifyNativeCodeLive(String name) {
    }
    
    public void write(Writer w) throws IOException {
        w.write("Live classes: " + liveClasses.size() + "\n");
        w.write("Live bytecode methods: " + liveBytecodeMethods + " out of " + loadedBytecodeMethods + "\n");
        w.write("Live bytecode bytes: " + liveBytecodeBytes + " out of " + loadedBytecodeBytes + "\n");
    }
}
