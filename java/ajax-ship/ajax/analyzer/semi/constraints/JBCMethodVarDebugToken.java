/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class JBCMethodVarDebugToken extends Token {
    private JBCMethod m;

    JBCMethodVarDebugToken(JBCMethod m) {
        this.m = m;
    }
    
    public String toString() {
        return m.toString();
    }
}
