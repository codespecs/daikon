/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class EmptyEnumerator implements Enumeration {
    private static final EmptyEnumerator enumerator = new EmptyEnumerator();
    
    public static EmptyEnumerator get() {
        return enumerator;
    }
    
    public EmptyEnumerator() {
    }
    
    public boolean hasMoreElements() {
        return false;
    }
    
    public Object nextElement() {
        throw new NoSuchElementException();
    }
}
