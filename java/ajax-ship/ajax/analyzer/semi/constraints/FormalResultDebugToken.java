/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class FormalResultDebugToken extends Token {
    private JBCMethod method;

    FormalResultDebugToken(JBCMethod method) {
        this.method = method;
    }
    
    public String toString() {
        return "Result for " + method;
    }
}
