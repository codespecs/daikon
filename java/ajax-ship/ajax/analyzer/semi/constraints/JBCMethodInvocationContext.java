/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.*;
import ajax.Globals;

class JBCMethodInvocationContext extends InvocationContext {
    private JBCMethod method;
    private int offset;
    
    private JBCMethodInvocationContext(InvocationContext parent, JBCMethod method, int offset) {
        super(parent);
        
        if (Globals.debug) {
            this.method = method;
            this.offset = offset;
        }
    }
    
    static JBCMethodInvocationContext get(InvocationContext parent, JBCMethod method, int offset) {
        return new JBCMethodInvocationContext(parent, method, offset);
    }
    
    public String toString() {
        return "base context";
    }
}
