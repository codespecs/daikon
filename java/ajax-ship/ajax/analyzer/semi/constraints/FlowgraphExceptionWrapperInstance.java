/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.*;
import ajax.Globals;

class FlowgraphExceptionWrapperInstance extends InstanceLabel {
    private Object container;
    private ExternalCFGNode node;
    
    private static final FlowgraphExceptionWrapperInstance singleton = new FlowgraphExceptionWrapperInstance(null, null);
    
    private FlowgraphExceptionWrapperInstance(Object container, ExternalCFGNode node) {
        if (Globals.debug) {
            this.container = container;
            this.node = node;
        }
    }
    
    static FlowgraphExceptionWrapperInstance get(Object container, ExternalCFGNode node) {
        if (ConstraintManager.makeExceptionComponents) {
            return new FlowgraphExceptionWrapperInstance(container, node);
        } else {
            return singleton;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return "exception wrapper at " + node + " in " + container;
        } else {
            return super.toString();
        }
    }
}
