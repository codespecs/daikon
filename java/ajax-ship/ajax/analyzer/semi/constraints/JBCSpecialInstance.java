/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.Globals;
import ajax.jbc.JBCMethod;

class JBCSpecialInstance extends InstanceLabel {
    private JBCMethod method;
    private int offset;
    
    private JBCSpecialInstance(JBCMethod method, int offset) {
        if (Globals.debug) {
            this.method = method;
            this.offset = offset;
        }
    }
    
    static JBCSpecialInstance get(JBCMethod m, int offset) {
        return new JBCSpecialInstance(m, offset);
    }
    
    public String toString() {
        if (Globals.debug) {
            return "special instance at offset " + offset + " in " + method;
        } else {
            return "special instance";
        }
    }
}
