/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class ActualReturnDebugToken extends Token {
    private JBCMethod m;
    private JBCMethod method;

    ActualReturnDebugToken(JBCMethod m, JBCMethod method) {
        this.m = m;
        this.method = method;
    }
    
    public String toString() {
        return "Actual return for call to " + m + " from " + method;
    }
}
