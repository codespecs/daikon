/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class JBCLocation extends Location {
    private JBCMethod method;
    private int offset;
    
    public JBCLocation(JBCMethod method, int offset) {
        this.method = method;
        this.offset = offset;
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public int getOffset() {
        return offset;
    }
    
    public int hashCode() {
        return method.hashCode()*18311 + offset*103147 + 19310;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCLocation) {
            JBCLocation l = (JBCLocation)o;
            
            return l.offset == offset && l.method.equals(method);
        } else {
            return false;
        }
    }
    
    public String toString() {
        return "Offset " + offset + " in " + method;
    }
    
    public JBCMethod getCalledMethod() {
        JBCMethod m = getMethod();
            
        return JBCCodeUtilities.resolveInstructionMethod(m,
            m.getData().getCode(), getOffset());
    }
    
    public JBCField getAccessedField() {
        JBCMethod m = getMethod();
            
        return JBCCodeUtilities.resolveInstructionField(m,
            m.getData().getCode(), getOffset());
    }
    
    public Object getFunction() {
        return method;
    }
}
