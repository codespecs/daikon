/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;
import ajax.jbc.JBCMethod;
import ajax.Globals;

public class JBCMethodInvocationInstance extends InstanceLabel {
    private InvocationContext context;
    private JBCMethod method;
    private int offset;
    
    private JBCMethodInvocationInstance(InvocationContext context,
        JBCMethod method, int offset) {
        this.method = method;
        this.offset = offset;
        
        if (Globals.debug) {
            this.context = context;
        }
    }
    
    public JBCMethod getCallerMethod() {
        return method;
    }
    
    public int getCallerOffset() {
        return offset;
    }
    
    static JBCMethodInvocationInstance get(InvocationContext context,
        JBCMethod method, int offset) {
        return new JBCMethodInvocationInstance(context, method, offset);
    }

    public String toString() {
        if (Globals.debug) {
            return "invocation at " + offset + " by " + method + " called by " + context;
        } else {
            return super.toString();
        }
    }
}
