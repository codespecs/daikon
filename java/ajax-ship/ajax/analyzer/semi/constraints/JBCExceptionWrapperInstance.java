/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;

class JBCExceptionWrapperInstance extends InstanceLabel {
    private JBCMethod method;
    private int offset;
    
    private static final JBCExceptionWrapperInstance singleton = new JBCExceptionWrapperInstance(null, 0);
    
    private JBCExceptionWrapperInstance(JBCMethod method, int offset) {
        if (Globals.debug) {
            this.method = method;
            this.offset = offset;
        }
    }
    
    static JBCExceptionWrapperInstance get(JBCMethod method, int offset) {
        if (ConstraintManager.makeExceptionComponents) {
            return new JBCExceptionWrapperInstance(method, offset);
        } else {
            return singleton;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return "exception wrapper at " + offset + " in " + method;
        } else {
            return super.toString();
        }
    }
}
