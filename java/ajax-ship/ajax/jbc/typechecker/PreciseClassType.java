/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

import ajax.util.*;
import ajax.jbc.*;

/**
The BytecodeTypechecker returns one of these types when precise class types
has been enabled. Normal JBCObjectTypes mean that the value could be of
that class or any subclass. A PreciseClassType means only that the value
could be of this specific class.
*/
public class PreciseClassType {
    private static WeakSet cache = new WeakSet();

    private JBCObjectType objectType;
    
    private PreciseClassType(JBCObjectType objectType) {
        this.objectType = objectType;
    }
    
    public static PreciseClassType get(JBCClass c) {
        return get(JBCObjectType.get(c));
    }
    
    public static PreciseClassType get(JBCObjectType objectType) {
        return (PreciseClassType)cache.add(new PreciseClassType(objectType));
    }
    
    public JBCObjectType getObjectType() {
        return objectType;
    }
    
    public boolean equals(Object o) {
        if (o instanceof PreciseClassType) {
            return ((PreciseClassType)o).objectType.equals(objectType);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return objectType.hashCode()*3811 + 10100;
    }
    
    public String toString() {
        return "Exactly " + objectType;
    }
}
