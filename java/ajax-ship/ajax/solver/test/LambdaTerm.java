/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.*;
import ajax.solver.*;

class LambdaTerm extends Term {
    private String arg;
    private Term body;
    
    LambdaTerm(String arg, Term body) {
        this.arg = arg;
        this.body = body;
    }
    
    public Variable makeType(World w, Environment env) {
        Variable v = new Variable(w);
        
        if (USEMODES) {
            env.pushBinding(arg, v.getComponent(w, Variable.CMODE, TypeComponent.get("arg")));
            v.getComponent(w, Variable.CMODE, TypeComponent.get("result"))
                .makeEqual(w, body.makeType(w, env));
        } else {
            env.pushBinding(arg, v.getComponent(w, TypeComponent.get("arg")));
            v.getComponent(w, TypeComponent.get("result"))
                .makeEqual(w, body.makeType(w, env));
        }
        
        env.popBinding();
        
        return v;
    }
    
    public String toString() {
        return "(\\" + arg + ". " + body.toString() + ")";
    }
}
