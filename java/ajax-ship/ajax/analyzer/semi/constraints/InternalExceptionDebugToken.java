/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class InternalExceptionDebugToken extends Token {
    private int offset;
    private JBCMethod method;

    InternalExceptionDebugToken(int offset, JBCMethod method) {
        this.offset = offset;
        this.method = method;
    }
    
    public String toString() {
        return "exception at " + offset + " in " + method;
    }
}
