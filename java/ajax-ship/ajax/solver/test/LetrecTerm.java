/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.Hashtable;
import ajax.solver.*;

class LetrecTerm extends Term {
    private String[] vars;
    private Term[] bindings;
    private Term body;
    
    LetrecTerm(String[] vars, Term[] bindings, Term body) {
        this.vars = vars;
        this.bindings = bindings;
        this.body = body;
    }
    
    public Variable makeType(World w, Environment env) {
        Variable[] vs = new Variable[vars.length];
        
        for (int i = 0; i < vs.length; i++) {
            vs[i] = new Variable(w);
            env.pushBinding(vars[i], vs[i]);
        }
        
        for (int i = 0; i < vs.length; i++) {
            vs[i].makeEqual(w, bindings[i].makeType(w, env));
        }
        
        Variable v = body.makeType(w, env);
        
        for (int i = 0; i < vs.length; i++) {
            env.popBinding();
        }
        
        return v;
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        
        buf.append("(letrec ");
        
        for (int i = 0; i < vars.length; i++) {
            if (i > 0) {
                buf.append(", ");
            }
            buf.append(vars[i]);
            buf.append("=");
            buf.append(bindings[i].toString());
        }
        
        buf.append(" in ");
        buf.append(body.toString());
        buf.append(")");
        
        return buf.toString();
    }
}
