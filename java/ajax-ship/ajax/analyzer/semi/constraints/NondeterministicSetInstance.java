/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;
import ajax.util.*;

class NondeterministicSetInstance extends InstanceLabel {
    private HashableCompactSet set;
    
    NondeterministicSetInstance(HashableCompactSet set) {
        if (Globals.debug) {
            this.set = set;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return "instance into set " + Globals.getHexID(set);
        } else {
            return super.toString();
        }
    }
}
