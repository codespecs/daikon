/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;

class SalamisNewDefNode extends SalamisDefNode implements ExternalCFGNewObjectDefNode {
    private String className;
    private SalamisFlowgraph fg;
    
    SalamisNewDefNode(SalamisParser p, String defVar, String className) {
        super(p, defVar);
        this.className = className;
    }

    public JBCClass getObjectClass() {
        return fg.resolveClass(className);
    }
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
    
    public String toString() {
        return "external creation of " + className;
    }
}
