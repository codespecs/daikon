/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.Hashtable;
import ajax.solver.*;

class PolyVarTerm extends Term {
    private String s;
    private String label;

    PolyVarTerm(String s, String label) {
        this.s = s;
        this.label = label;
    }

    public Variable makeType(World w, Environment env) {
        return env.lookup(s).getInstance(w, new PolyInstance(label));
    }

    public String toString() {
        return s;
    }
}
