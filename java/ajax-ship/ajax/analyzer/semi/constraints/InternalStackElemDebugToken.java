/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class InternalStackElemDebugToken extends Token {
    private int index;
    private int offset;
    private JBCMethod method;

    InternalStackElemDebugToken(int index, int offset, JBCMethod method) {
        this.index = index;
        this.offset = offset;
        this.method = method;
    }
    
    public String toString() {
        return "Stack elem #" + index + " at " + offset + " in " + method;
    }
}
