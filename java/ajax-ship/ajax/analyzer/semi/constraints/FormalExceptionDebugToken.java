/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class FormalExceptionDebugToken extends Token {
    private JBCMethod method;

    FormalExceptionDebugToken(JBCMethod method) {
        this.method = method;
    }
    
    public String toString() {
        return "Exception for " + method;
    }
}
