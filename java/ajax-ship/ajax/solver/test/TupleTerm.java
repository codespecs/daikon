/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.*;
import java.util.Hashtable;

class TupleTerm extends Term {
    private Term[] terms;
    
    TupleTerm(Term[] terms) {
        this.terms = terms;
    }
    
    public Variable makeType(World w, Environment env) {
        Variable v = new Variable(w);
        
        for (int i = 0; i < terms.length; i++) {
            if (USEMODES) {
                v.getComponent(w, Variable.CMODE, TypeComponent.get("#" + i))
                    .makeEqual(w, terms[i].makeType(w, env));
            } else {
                v.getComponent(w, TypeComponent.get("#" + i))
                    .makeEqual(w, terms[i].makeType(w, env));
            }
        }
            
        return v;
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        
        buf.append("[");
        for (int i = 0; i < terms.length; i++) {
            if (i > 0) {
                buf.append(", ");
            }
            
            buf.append(terms[i].toString());
        }
        buf.append("]");
        
        return buf.toString();
    }
}
