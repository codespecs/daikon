/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.util.Enumeration;

public class HashableCompactSet extends CompactSet {
    private int hashValue = 109401187;
    
    public HashableCompactSet(int initialSize, Enumeration initialElements) {
        super(initialSize, initialElements);
    }
    
    protected void notifyInitialElementAdded(Object obj) {
        super.notifyInitialElementAdded(obj);
        hashValue ^= obj.hashCode();
    }
    
    public HashableCompactSet() {
        super();
    }
    
    public int hashCode() {
        return hashValue;
    }
    
    public void addUnconditionally(Object obj) {
        super.addUnconditionally(obj);
        hashValue ^= obj.hashCode();
    }
    
    public Object remove(Object obj) {
        Object result = super.remove(obj);
        
        if (result != null) {
            hashValue ^= obj.hashCode();
        }
        
        return result;
    }
    
    public boolean equals(Object o) {
        return hashValue == o.hashCode() && super.equals(o);
    }
}
