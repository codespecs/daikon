/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;
import ajax.jbc.JBCClass;

class JBCClassVarDebugToken extends Token {
    private JBCClass c;

    JBCClassVarDebugToken(JBCClass c) {
        this.c = c;
    }
    
    public String toString() {
        return c.toString();
    }
}
