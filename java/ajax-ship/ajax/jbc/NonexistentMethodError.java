/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public class NonexistentMethodError extends Error {
    private JBCMethod method;
    
    NonexistentMethodError(JBCMethod method) {
        super("Method does not exist");
        this.method = method;
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public String getMessage() {
        return "Method " + method + " does not exist";
    }
}
