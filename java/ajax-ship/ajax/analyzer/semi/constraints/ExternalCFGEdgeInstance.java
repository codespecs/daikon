/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.ExternalCFGNode;
import ajax.Globals;

class ExternalCFGEdgeInstance extends InstanceLabel {
    private ExternalCFGNode from;
    private ExternalCFGNode to;
    
    ExternalCFGEdgeInstance(ExternalCFGNode from, ExternalCFGNode to) {
        if (Globals.debug) {
            this.from = from;
            this.to = to;
        }
    }
}
