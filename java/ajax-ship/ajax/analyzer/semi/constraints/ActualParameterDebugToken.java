/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class ActualParameterDebugToken extends Token {
    private int i;
    private JBCMethod m;
    private JBCMethod method;

    ActualParameterDebugToken(int i, JBCMethod m, JBCMethod method) {
        this.i = i;
        this.m = m;
        this.method = method;
    }
    
    public String toString() {
        return "Actual parameter " + i + " for call to " + m + " from " + method;
    }
}
