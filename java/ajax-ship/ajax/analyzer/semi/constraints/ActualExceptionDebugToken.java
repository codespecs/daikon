/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class ActualExceptionDebugToken extends Token {
    private JBCMethod m;
    private JBCMethod method;

    ActualExceptionDebugToken(JBCMethod m, JBCMethod method) {
        this.m = m;
        this.method = method;
    }
    
    public String toString() {
        return "Actual exception for call to " + m + " from " + method;
    }
}
