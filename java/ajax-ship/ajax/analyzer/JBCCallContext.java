/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.*;

public class JBCCallContext {
    private JBCExpressionContext callerContext;
    private JBCMethod method;
    private int offset;
    
    JBCCallContext(JBCExpressionContext callerContext, JBCMethod m, int offset) {
        this.callerContext = callerContext;
        this.method = m;
        this.offset = offset;
    }
    
    public JBCExpressionContext getCallerContext() {
        return callerContext;
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public int getOffset() {
        return offset;
    }
    
    static JBCCallContext get(JBCExpressionContext context, JBCMethod m, int offset) {
        return new JBCCallContext(context, m, offset);
    }
}
