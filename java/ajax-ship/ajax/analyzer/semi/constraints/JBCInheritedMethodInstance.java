/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.*;
import ajax.Globals;

class JBCInheritedMethodInstance extends InstanceLabel {
    private JBCClass c;
    private JBCMethod m;
    
    JBCInheritedMethodInstance(JBCClass c, JBCMethod m) {
        if (Globals.debug) {
            this.c = c;
            this.m = m;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return "inheritance by " + c + " as " + m;
        } else {
            return "inheritance of a method";
        }
    }
}
