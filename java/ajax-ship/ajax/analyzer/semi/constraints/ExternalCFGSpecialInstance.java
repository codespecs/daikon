/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.Globals;
import ajax.jbc.ExternalCFGNode;

class ExternalCFGSpecialInstance extends InstanceLabel {
    private ExternalCFGNode node;
    private int delta;
    
    private ExternalCFGSpecialInstance(ExternalCFGNode node, int delta) {
        if (Globals.debug) {
            this.node = node;
            this.delta = delta;
        }
    }
    
    static ExternalCFGSpecialInstance get(ExternalCFGNode node, int delta) {
        return new ExternalCFGSpecialInstance(node, delta);
    }   
}
