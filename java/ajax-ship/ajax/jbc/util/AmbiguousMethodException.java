/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.JBCMethod;

public class AmbiguousMethodException extends Exception {
    private JBCMethod method;
    
    public AmbiguousMethodException(String s, JBCMethod method) {
        super(s);
        this.method = method;
    }
    
    public JBCMethod getOneMethod() {
        return method;
    }
    
    public String getClassName() {
        return method.getContainingClass().getClassName();
    }
}
