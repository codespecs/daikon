/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;

class JBCExceptionThrowInstance extends InstanceLabel {
    private JBCMethod method;
    private int offset;
    
    private static final JBCExceptionThrowInstance singleton = new JBCExceptionThrowInstance(null, 0);
    
    private JBCExceptionThrowInstance(JBCMethod method, int offset) {
        if (Globals.debug) {
            this.method = method;
            this.offset = offset;
        }
    }
    
    static JBCExceptionThrowInstance get(JBCMethod method, int offset) {
        if (ConstraintManager.makeExceptionComponents) {
            return new JBCExceptionThrowInstance(method, offset);
        } else {
            return singleton;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return "exception catch at " + offset + " in " + method;
        } else {
            return super.toString();
        }
    }
}
