/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.Vector;

public class JBCObserverMulticaster implements JBCObserver {
    private JBCObserver[] os;
    
    private static void addObservers(Vector list, JBCObserver o) {
        if (o instanceof JBCObserverMulticaster) {
            JBCObserverMulticaster multi = (JBCObserverMulticaster)o;
            JBCObserver[] os = multi.os;
            
            for (int i = 0; i < os.length; i++) {
                list.addElement(os[i]);
            }
        } else {
            list.addElement(o);
        }
    }
    
    public JBCObserverMulticaster(JBCObserver o1, JBCObserver o2) {
        Vector list = new Vector();
        
        addObservers(list, o1);
        addObservers(list, o2);
        
        os = new JBCObserver[list.size()];
        list.copyInto(os);
    }
    
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyClassChanged(c, from, to);
        }
    }
    
    public void notifyAddedMainInvocation(String name) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyAddedMainInvocation(name);
        }
    }
    
    public void notifyClassLoaded(JBCClass c) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyClassLoaded(c);
        }
    }
    
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyNativeCodeLoaded(m, fg);
        }
    }
    
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyNativeCodeLoaded(name, fg);
        }
    }
    
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyNativeCodeChanged(m, from, to);
        }
    }
    
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to) {
        for (int i = 0; i < os.length; i++) {
            os[i].notifyNativeCodeChanged(name, from, to);
        }
    }
}
