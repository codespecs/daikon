/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.protocol;

import java.io.*;

public class ClassDescriptor implements Serializable {
    private String name;
    private String sourceFileName;
    
    public ClassDescriptor(String name, String sourceFileName) {
        this.name = name;
        this.sourceFileName = sourceFileName;
    }
    
    public String getClassName() {
        return name;
    }

    public String getSourceFileName() {
        return sourceFileName;
    }
    
    public int hashCode() {
        return name.hashCode()*394101 + 1001001017;
    }
    
    public boolean equals(Object o) {
        if (o instanceof ClassDescriptor) {
            ClassDescriptor c = (ClassDescriptor)o;
            
            return c.name.equals(name);
        } else {
            return false;
        }
    }
    
    public String toString() {
        return name;
    }
}
