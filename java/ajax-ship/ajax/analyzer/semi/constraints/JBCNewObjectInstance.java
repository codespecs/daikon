/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;

class JBCNewObjectInstance extends InstanceLabel {
    private InvocationContext context;
    private JBCMethod method;
    private int offset;
    
    private JBCNewObjectInstance(InvocationContext context, JBCMethod method, int offset) {
        if (Globals.debug) {
            this.context = context;
            this.method = method;
            this.offset = offset;
        }
    }       
    
    public static JBCNewObjectInstance get(InvocationContext context,
        JBCMethod method, int offset) {
        return new JBCNewObjectInstance(context, method, offset);
    }
    
    public String toString() {
        if (Globals.debug) {
            return "new object at " + offset + " in " + method;
        } else {
            return "new object";
        }
    }
}
