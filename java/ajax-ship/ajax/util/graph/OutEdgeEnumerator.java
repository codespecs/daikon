/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;
import ajax.util.*;
import ajax.Globals;

class OutEdgeEnumerator implements Enumeration {
    private Enumeration e;
    private Enumeration setEnum = null;
    private Object next = null;
    
    OutEdgeEnumerator(Enumeration e) {
        this.e = e;
    }
    
    private void advanceToNextElement() {
        if (next == null) {
            while (setEnum != null || e.hasMoreElements()) {
                if (setEnum != null && setEnum.hasMoreElements()) {
                    next = setEnum.nextElement();
                    return;
                }
                
                if (e.hasMoreElements()) {
                    next = e.nextElement();
                    if (next instanceof IdentityCompactSet) {
                        setEnum = ((IdentityCompactSet)next).elements();
                        
                        if (Globals.debug && !setEnum.hasMoreElements()) {
                            Globals.localError("Out edge set is empty");
                        }
                        
                        next = setEnum.nextElement();
                    }
                    return;
                } else {
                    setEnum = null;
                    next = null;
                }
            }
        }
    }
    
    public boolean hasMoreElements() {
        advanceToNextElement();
        
        return next != null;
    }
    
    public Object nextElement() {
        advanceToNextElement();
        
        if (next == null) {
            throw new NoSuchElementException();
        } else {
            Object result = next;
            
            next = null;
            return result;
        }
    }
}
