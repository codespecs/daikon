/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.reflect;

import ajax.util.*;
import ajax.jbc.*;

class ReflectionData  {
    private int kind;
    private Object data;
    
    public static final int REFLECTED_METHOD = 1;
    public static final int REFLECTED_CLASS  = 2;
    public static final int SERIALIZED_CLASS = 3;
    
    ReflectionData(JBCClass c) {
        this(REFLECTED_CLASS, c);
    }
    
    ReflectionData(JBCMethod m) {
        this(REFLECTED_METHOD, m);
    }
    
    ReflectionData(int kind, Object data) {
        this.kind = kind;
        this.data = data;
    }
    
    int getKind() {
        return kind;
    }
    
    Object getData() {
        return data;
    }
    
    public int hashCode() {
        return kind*10840107 + data.hashCode();
    }
    
    public boolean equals(Object o) {
        if (o instanceof ReflectionData) {
            ReflectionData r = (ReflectionData)o;
            
            return r.kind == kind && r.data.equals(data);
        } else {
            return false;
        }
    }
}
