/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.Hashtable;
import ajax.solver.*;

class VarTerm extends Term {
    private String s;
    
    VarTerm(String s) {
        this.s = s;
    }
    
    public Variable makeType(World w, Environment env) {
        return env.lookup(s);
    }
    
    public String toString() {
        return s;
    }
}
