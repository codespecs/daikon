/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public interface JBCObserver {
    public void notifyClassLoaded(JBCClass c);
    public void notifyClassChanged(JBCClass c, ClassData from, ClassData to);
    public void notifyNativeCodeLoaded(JBCMethod m, ExternalFlowgraph fg);
    public void notifyNativeCodeLoaded(String name, ExternalFlowgraph fg);
    public void notifyNativeCodeChanged(JBCMethod m, ExternalFlowgraph from, ExternalFlowgraph to);
    public void notifyNativeCodeChanged(String name, ExternalFlowgraph from, ExternalFlowgraph to);
    public void notifyAddedMainInvocation(String name);
}
