/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.*;
import java.util.Hashtable;

class ProjectionTerm extends Term {
    private Term t;
    private int index;
    
    ProjectionTerm(Term t, int index) {
        this.t = t;
        this.index = index;
    }
    
    public Variable makeType(World w, Environment env) {
        if (USEMODES) {
            return t.makeType(w, env)
                .getComponent(w, Variable.DMODE, TypeComponent.get("#" + index));
        } else {
            return t.makeType(w, env)
                .getComponent(w, TypeComponent.get("#" + index));
        }
    }
    
    public String toString() {
        return "(#" + index + " " + t.toString() + ")";
    }
}
