/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

abstract public class JBCNativeCodeLoader {
    private JBCWorld world;
    
    public JBCNativeCodeLoader(JBCWorld world) {
        this.world = world;
        
        world.addNativeCodeLoader(this);
    }
    
    public JBCWorld getWorld() {
        return world;
    }

/**
Override this.
*/
    abstract protected ExternalFlowgraph loadMethodCode(JBCMethod method);
/**
Override this.
*/
    abstract protected ExternalFlowgraph loadFunctionCode(String name);
    
/**
The native code for the method being set is loaded by calling
loadMethodCode. Note that this will not change the code if some
JBCNativeClassLoader earlier on the list is already providing the code.
*/
    protected void setMethodCode(JBCMethod method) {
        world.setMethodNativeCode(method);
    }
    
/**
The native code for the function being set is loaded by calling
loadFunctionCode. Note that this will not change the code if some
JBCNativeClassLoader earlier on the list is already providing the code.
*/
    protected void setFunctionCode(String name) {
        world.setFunctionNativeCode(name);
    }
}
