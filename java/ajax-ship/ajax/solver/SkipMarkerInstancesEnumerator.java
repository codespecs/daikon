/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import java.util.*;

class SkipMarkerInstancesEnumerator implements Enumeration {
    private Enumeration baseEnumerator;
    private InstantiationRecord next = null;
    
    SkipMarkerInstancesEnumerator(Enumeration baseEnumerator) {
        this.baseEnumerator = baseEnumerator;
    }
    
    private void setNext() {
        if (next == null) {
            if (baseEnumerator != null) {
                while (baseEnumerator.hasMoreElements()) {
                    next = (InstantiationRecord)baseEnumerator.nextElement();
                    
                    if (next.to != null) {
                        return;
                    }
                }
                baseEnumerator = null;
                next = null;
            }
        }
    }
    
    public boolean hasMoreElements() {
        setNext();
        return next != null;
    }
    
    public Object nextElement() {
        setNext();
        if (next == null) {
            throw new NoSuchElementException();
        } else {
            Object result = next;

            next = null;
            return result;
        }
    }
}
