/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class EnumerationMap implements Enumeration {
    private EnumerationMapper mapper;
    private Enumeration e;
    private Object next = null;
    
    public EnumerationMap(EnumerationMapper mapper, Enumeration e) {
        this.mapper = mapper;
        this.e = e;
    }
    
    protected EnumerationMap(Enumeration e) {
        this(null, e);
    }
    
    protected Object map(Object o) {
        return mapper.map(o);
    }
    
    private void advanceToNextElement() {
        while (next == null && e != null) {
            if (e.hasMoreElements()) {
                next = map(e.nextElement());
            } else {
                e = null;
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
