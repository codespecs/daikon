/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class FormalGlobalsDebugToken extends Token {
    private JBCMethod method;

    FormalGlobalsDebugToken(JBCMethod method) {
        this.method = method;
    }
    
    public String toString() {
        return "Globals for " + method;
    }
}
