/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.ExternalCFGNode;
import ajax.Globals;

class ExternalCFGMethodInvocationInstance extends InstanceLabel {
    private ExternalCFGNode caller;
    
    ExternalCFGMethodInvocationInstance(ExternalCFGNode caller) {
        if (Globals.debug) {
            this.caller = caller;
        }
    }
    
    static ExternalCFGMethodInvocationInstance get(ExternalCFGNode caller) {
        return new ExternalCFGMethodInvocationInstance(caller);
    }
    
    public String toString() {
        return "call from " + caller.toString();
    }
}
