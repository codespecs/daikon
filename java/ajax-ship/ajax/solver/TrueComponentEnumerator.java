/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import java.util.*;

class TrueComponentEnumerator implements Enumeration {
    private Enumeration source;
    private ComponentRecord next = null;
    
    TrueComponentEnumerator(Enumeration source) {
        this.source = source;
    }
    
    private void advanceToElement() {
        while (next == null && source.hasMoreElements()) {
            next = (ComponentRecord)source.nextElement();
            
            if (next.getKind() != ComponentRecord.COMPONENT) {
                next = null;
            }
        }
    }
    
    public boolean hasMoreElements() {
        advanceToElement();
        return next != null;
    }
    
    public Object nextElement() {
        advanceToElement();
        if (next == null) {
            throw new NoSuchElementException();
        } else {
            ComponentRecord result = next;
            
            next = null;
            return result;
        }
    }
}
