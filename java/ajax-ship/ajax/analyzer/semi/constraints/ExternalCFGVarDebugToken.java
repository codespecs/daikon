/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.analyzer.semi.Token;

class ExternalCFGVarDebugToken extends Token {
    private String name;

    ExternalCFGVarDebugToken(String name) {
        this.name = name;
    }
    
    public String toString() {
        return name;
    }
}
