/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

import ajax.jbc.JBCType;

public class ReturnAddrType extends JBCType {
    private int returnTo;
    
    public ReturnAddrType(int returnTo) {
        this.returnTo = returnTo;
    }
    
    public int hashCode() {
        return returnTo*10311 + 104901;
    }
    
    public boolean equals(Object o) {
        if (o instanceof ReturnAddrType) {
            ReturnAddrType t = (ReturnAddrType)o;
            
            return t.returnTo == returnTo;
        } else {
            return false;
        }
    }
    
    public boolean isEqualType(JBCType t) {
        return t instanceof ReturnAddrType;
    }
    
    public int getWordSize() {
        return 1;
    }
    
    public int getReturnAddress() {
        return returnTo;
    }
    
    public String toString() {
        return "Return-to-" + returnTo;
    }
}
