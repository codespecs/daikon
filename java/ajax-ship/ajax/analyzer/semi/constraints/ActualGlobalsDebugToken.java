/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class ActualGlobalsDebugToken extends Token {
    private JBCMethod m;
    private JBCMethod method;

    ActualGlobalsDebugToken(JBCMethod m, JBCMethod method) {
        this.m = m;
        this.method = method;
    }
    
    public String toString() {
        return "Actual globals for call to " + m + " from " + method;
    }
}
