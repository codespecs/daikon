/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.util.graph.*;
import ajax.jbc.*;

class CallGraphNode implements GraphNode {
    private Object function;
    
    CallGraphNode(Object function) {
        this.function = function;
    }
    
    Object getFunction() {
        return function;
    }

    public String toString() {
        if (function instanceof JBCMethod) {
            JBCMethod m = (JBCMethod)function;
            String className = m.getContainingClass().getClassName();
            int lastDot = className.lastIndexOf('.');
                    
            if (lastDot >= 0) {
                className = className.substring(lastDot + 1);
            }
                  
            return className + "." + m.getMethodName();
        } else {
            return (String)function;
        }
    }
}
