/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.ExternalCFGNewObjectDefNode;
import ajax.Globals;

class ExternalCFGNewObjectInstance extends InstanceLabel {
    private ExternalCFGNewObjectDefNode caller;
    
    ExternalCFGNewObjectInstance(ExternalCFGNewObjectDefNode caller) {
        if (Globals.debug) {
            this.caller = caller;
        }
    }
    
    static ExternalCFGNewObjectInstance get(ExternalCFGNewObjectDefNode caller) {
        return new ExternalCFGNewObjectInstance(caller);
    }
    
    public String toString() {
        if (Globals.debug) {
            return "object creation by " + caller;
        } else {
            return "external new object creation";
        }
    }
}
