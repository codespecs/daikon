/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

public class Token {
    Token nextToken = null;
    Token prevToken = null;
    
    public Token() {
    }
    
    public Token getNextToken() {
        return nextToken;
    }
}
