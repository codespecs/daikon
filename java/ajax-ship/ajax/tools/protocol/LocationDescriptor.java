/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class LocationDescriptor implements Serializable {
    private MethodDescriptor m;
    private int offset;
    private int lineNum;
    
    public LocationDescriptor(MethodDescriptor m, int offset, int lineNum) {
        this.m = m;
        this.offset = offset;
        this.lineNum = lineNum;
    }
    
    public MethodDescriptor getMethodDescriptor() {
        return m;
    }
    
    public int getOffset() {
        return offset;
    }

    public int getLineNumber() {
        return lineNum;
    }
    
    public int hashCode() {
        return m.hashCode()*30101 + offset;
    }
    
    public boolean equals(Object o) {
        if (o instanceof LocationDescriptor) {
            LocationDescriptor l = (LocationDescriptor)o;
            
            return l.m.equals(m) && l.offset == offset;
        } else {
            return false;
        }
    }
    
    public String toString() {
        return m.toString() + "#" + offset;
    }
}
