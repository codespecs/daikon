/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.util.graph.*;

class DependenceFunctionNode implements GraphNode {
    private Object function;
    
    DependenceFunctionNode(Object function) {
        this.function = function;
    }
    
    Object getFunction() {
        return function;
    }
    
    public String toString() {
        return function.toString();
    }
}
