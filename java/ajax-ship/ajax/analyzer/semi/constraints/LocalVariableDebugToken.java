/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCMethod;

class LocalVariableDebugToken extends Token {
    private int index;
    private int offset;
    private JBCMethod method;

    LocalVariableDebugToken(int index, int offset, JBCMethod method) {
        this.index = index;
        this.offset = offset;
        this.method = method;
    }
    
    public String toString() {
        return "local variable #" + index + " at " + offset + " in " + method;
    }
}
