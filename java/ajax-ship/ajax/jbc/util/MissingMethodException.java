/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

public class MissingMethodException extends Exception {
    private String methodName;
    private String className;
    
    public MissingMethodException(String s, String className, String methodName) {
        super(s);
        this.methodName = methodName;
        this.className = className;
    }
    
    public String getClassName() {
        return className;
    }
    
    public String getMethodName() {
        return methodName;
    }
}
