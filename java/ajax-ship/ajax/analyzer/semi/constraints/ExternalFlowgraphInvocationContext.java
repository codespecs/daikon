/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;
import ajax.Globals;

class ExternalFlowgraphInvocationContext extends InvocationContext {
    private ExternalFlowgraph fg;
    private ExternalCFGNode node;
    
    private ExternalFlowgraphInvocationContext(InvocationContext parent, ExternalFlowgraph fg, ExternalCFGNode node) {
        super(parent);
        
        if (Globals.debug) {
            this.fg = fg;
            this.node = node;
        }
    }
    
    static ExternalFlowgraphInvocationContext get(InvocationContext parent, ExternalFlowgraph fg, ExternalCFGNode node) {
        return new ExternalFlowgraphInvocationContext(parent, fg, node);
    }
}
