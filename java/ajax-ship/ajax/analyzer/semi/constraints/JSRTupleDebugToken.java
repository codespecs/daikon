/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class JSRTupleDebugToken extends Token {
    private int i;
    private JBCMethod m;

    JSRTupleDebugToken(int i, JBCMethod m) {
        this.i = i;
        this.m = m;
    }
    
    public String toString() {
        return "jsr tuple for " + i + " in " + m;
    }
}
