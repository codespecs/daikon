/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import java.util.*;
import ajax.solver.Variable;
import java.io.*;
import ajax.solver.util.ConstraintAnnotator;
import ajax.util.IdentityHashtable;

class TokenManager {
    private IdentityHashtable tokenTable = new IdentityHashtable();
    
    TokenManager() {
    }
    
    void addToken(Variable v, Token t) {
        Variable vHead = v.getHead();
        Token curToken = (Token)tokenTable.get(vHead);
        
        if (curToken == null) {
            t.nextToken = null;
            t.prevToken = t;
            
            tokenTable.put(vHead, t);
        } else {
            Token lastToken = curToken.prevToken;
            
            lastToken.nextToken = t;
            t.prevToken = lastToken;
            t.nextToken = null;
            curToken.prevToken = t;
        }
    }
    
    void removeToken(Variable v, Token t) {
        Variable vHead = v.getHead();
        Token firstToken = (Token)tokenTable.get(vHead);
        
        if (firstToken == t) {
            if (t.nextToken != null) {
                tokenTable.put(vHead, t.nextToken);
            } else {
                tokenTable.remove(vHead);
            }
        }
        
        if (t.nextToken == null) {
            firstToken.prevToken = t.prevToken;
        } else {
            t.nextToken.prevToken = t.prevToken;
        }
        
        t.prevToken.nextToken = t.nextToken;
    }
    
    Token getFirstToken(Variable v) {
        return (Token)tokenTable.get(v.getHead());
    }    
    
    public static int countTokens(Token t) {
        int result = 0;
        
        while (t != null) {
            result++;
            t = t.nextToken;
        }
        
        return result;
    }
    
    void notifyVarsMerged(Variable toHead, Variable fromHead) {
        Token firstFromToken = (Token)tokenTable.remove(fromHead);
        
        if (firstFromToken != null) {
            Token firstToToken = (Token)tokenTable.get(toHead);
            
            if (firstToToken == null) {
                tokenTable.put(toHead, firstFromToken);
            } else {
                firstFromToken.prevToken.nextToken = firstToToken.nextToken;
                if (firstToToken.nextToken == null) {
                    firstToToken.prevToken = firstFromToken.prevToken;
                } else {
                    firstToToken.nextToken.prevToken = firstFromToken.prevToken;
                }
                firstFromToken.prevToken = firstToToken;
                firstToToken.nextToken = firstFromToken;
            }
        }
    }
    
    public String getVarLabel(Variable v) {
        Token t = (Token)tokenTable.get(v.getHead());
        String result;
        
        if (t == null) {
            result = "{" + v.getClusterID() + "}";
        } else {
            int line_len = (int)Math.sqrt(countTokens(t)) + 1;
            StringBuffer buf = new StringBuffer();
            
            for (; t != null; t = t.nextToken) {
                buf.append(t.toString()).append("\n");
            }
            
            buf.append("{").append(v.getClusterID()).append("}");
            result = buf.toString();
        }
        
        return result;
    }
    
    public Enumeration getTokenVars() {
        return tokenTable.keys();
    }
}
