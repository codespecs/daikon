/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class SingletonEnumerator implements Enumeration {
    private Object obj;
    
    public SingletonEnumerator(Object obj) {
        this.obj = obj;
    }
    
    public boolean hasMoreElements() {
        return obj != null;
    }
    
    public Object nextElement() {
        if (obj != null) {
            Object result = obj;
            
            obj = null;
            return result;
        } else {
            throw new NoSuchElementException();
        }
    }
}
