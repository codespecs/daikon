/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.jbc.ExternalCFGNode;
import ajax.Globals;

class ExternalCFGNodeComponent extends ComponentLabel {
    private ExternalCFGNode node;
    
    ExternalCFGNodeComponent(ExternalCFGNode node) {
        if (Globals.debug) this.node = node;
    }
}
