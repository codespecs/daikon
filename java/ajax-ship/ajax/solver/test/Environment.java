/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import java.util.*;
import ajax.solver.*;

class Environment {
    private Hashtable bindings = new Hashtable();
    private Stack savedBindings = new Stack();
    private Vector bindingList = new Vector();

    void pushBinding(String s, Variable v) {
        Object[] pair = { s, bindings.get(s) };

        savedBindings.push(pair);
        bindings.put(s, v);
        
        Object[] savedPair = { s, v };
        
        bindingList.addElement(savedPair);
    }

    void popBinding() {
        Object[] pair = (Object[])savedBindings.pop();

        if (pair[1] == null) {
            bindings.remove(pair[0]);
        } else {
            bindings.put(pair[0], pair[1]);
        }
    }
    
    Variable lookup(String s) {
        Variable result = (Variable)bindings.get(s);
        
        if (result == null) {
            throw new IllegalArgumentException("Unbound variable: " + s);
        } else {
            return result;
        }
    }
    
    Vector getBindingList() {
        return bindingList;
    }
}
