/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.JBCMethod;
import ajax.Globals;

class JBCInvocationContext extends InvocationContext {
    private JBCMethod method;
    private int offset;
    
    private JBCInvocationContext(InvocationContext parent, JBCMethod method, int offset) {
        super(parent);
        
        if (Globals.debug) {
            this.offset = offset;
            this.method = method;
        }
    }
    
    static JBCInvocationContext get(InvocationContext parent, JBCMethod method, int offset) {
        return new JBCInvocationContext(parent, method, offset);
    }
    
    public String toString() {
        if (Globals.debug) {
            return Integer.toString(offset) + " in " + method;
        } else {
            return "<invocation context>";
        }
    }
}
