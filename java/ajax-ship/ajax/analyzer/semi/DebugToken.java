/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

public class DebugToken extends Token {
    private String s;

    public DebugToken(String s) {
        this.s = s;
    }
    
    public String toString() {
        return s;
    }
}
