/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import ajax.util.*;
import java.util.Enumeration;
import java.io.Serializable;

class SalamisDefNode extends SalamisNode implements ExternalCFGDefNode {
    ExternalCFGNode next = null;
    protected ExternalCFGVariable defVar;
    
    SalamisDefNode(SalamisParser p, String varName) {
        this.defVar = p.defineVar(varName, this);
    }
    
    public Enumeration getSuccessors() {
        if (next != null) {
            return new SingletonEnumerator(next);
        } else {
            return new EmptyEnumerator();
        }
    }
    
    public ExternalCFGVariable[] getDefinedVariables() {
        ExternalCFGVariable[] defVars = { defVar };
        
        return defVars;
    }
}
