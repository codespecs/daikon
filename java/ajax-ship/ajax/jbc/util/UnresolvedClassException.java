/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

public class UnresolvedClassException extends Exception {
    private String className;
    
    public UnresolvedClassException(String className, String s) {
        super(s);
        
        this.className = className;
    }
    
    public String getClassName() {
        return className;
    }
}
