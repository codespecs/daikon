/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.Hashtable;
import ajax.solver.*;

class AppTerm extends Term {
    private Term fun;
    private Term arg;
    
    AppTerm(Term fun, Term arg) {
        this.fun = fun;
        this.arg = arg;
    }
    
    public Variable makeType(World w, Environment env) {
        Variable funV = fun.makeType(w, env);
        
        if (USEMODES) {
            funV.getComponent(w, Variable.DMODE, TypeComponent.get("arg"))
                .makeEqual(w, arg.makeType(w, env));
            
            return funV.getComponent(w, Variable.DMODE, TypeComponent.get("result"));
        } else {
            funV.getComponent(w, TypeComponent.get("arg"))
                .makeEqual(w, arg.makeType(w, env));
            
            return funV.getComponent(w, TypeComponent.get("result"));
        }
    }
    
    public String toString() {
        return fun.toString() + " " + arg.toString();
    }
}
