/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.ExternalCFGFlowgraphInvocationDefNode;
import ajax.Globals;

class ExternalCFGFlowgraphInvocationInstance extends InstanceLabel {
    private ExternalCFGFlowgraphInvocationDefNode caller;
    
    ExternalCFGFlowgraphInvocationInstance(ExternalCFGFlowgraphInvocationDefNode caller) {
        if (Globals.debug) {
            this.caller = caller;
        }
    }
}
