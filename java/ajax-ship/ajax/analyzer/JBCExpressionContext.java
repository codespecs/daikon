/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.*;

public class JBCExpressionContext {
    private static JBCExpressionContext baseContext = new JBCExpressionContext();
    
    JBCExpressionContext() {
    }
    
    public static JBCExpressionContext getUniversalContext() {
        return baseContext;
    }
    
    public JBCCallContext getCallContext(JBCMethod m, int offset) {
        return JBCCallContext.get(this, m, offset);
    }
}
