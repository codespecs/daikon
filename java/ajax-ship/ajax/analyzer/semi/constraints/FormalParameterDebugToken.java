/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class FormalParameterDebugToken extends Token {
    private int i;
    private JBCMethod method;

    FormalParameterDebugToken(int i, JBCMethod method) {
        this.i = i;
        this.method = method;
    }
    
    public String toString() {
        return "Parameter " + i + " for " + method;
    }
}
