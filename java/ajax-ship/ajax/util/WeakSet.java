/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

/**
This class maintains a set of objects with the property that objects
in the set can be garbage collected, in which case they are removed
from the set.
*/
public class WeakSet {
    private CompactSet table = new CompactSet();
    
    public Enumeration elements() {
        return table.elements();
    }
    
    public Object add(Object obj) {
        Object existing = table.get(obj);
        
        if (existing != null) {
            return existing;
        } else {
            table.addUnconditionally(obj);
            return obj;
        }
    }
}
