/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.Globals;

class InvocationContextComponent extends ComponentLabel {
    private InvocationContext context;
    
    InvocationContextComponent(InvocationContext context) {
        if (Globals.debug) this.context = context;
    }
    
    public String toString() {
        if (Globals.debug) {
            return context.toString();
        } else {
            return "<invocation context>";
        }
    }
}
