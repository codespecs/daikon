/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class MethodDescriptor implements Serializable {
    private ClassDescriptor c;
    private String methodName;
    private String methodSignature;
    
    public MethodDescriptor(ClassDescriptor c, String methodName, String methodSignature) {
        this.c = c;
        this.methodName = methodName;
        this.methodSignature = methodSignature;
    }
    
    public ClassDescriptor getClassDescriptor() {
        return c;
    }
    
    public String getMethodName() {
        return methodName;
    }
    
    public String getMethodSignature() {
        return methodSignature;
    }
    
    public int hashCode() {
        return c.hashCode()*301401 + methodName.hashCode()*10101 + methodSignature.hashCode() + 14901781;
    }
    
    public boolean equals(Object o) {
        if (o instanceof MethodDescriptor) {
            MethodDescriptor m = (MethodDescriptor)o;
            
            return m.c.equals(c) && m.methodName.equals(methodName)
                && m.methodSignature.equals(methodSignature);
        } else {
            return false;
        }
    }
    
    public String toString() {
        return c.toString() + "." + methodName;
    }
}
