/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class ArrayEnumerator implements Enumeration {
    private Object[] array;
    private int curIndex = 0;
    
    public ArrayEnumerator(Object[] array) {
        this.array = array;
    }
    
    public boolean hasMoreElements() {
        return curIndex < array.length;
    }
    
    public Object nextElement() {
        try {
            return array[curIndex++];
        } catch (ArrayIndexOutOfBoundsException ex) {
            throw new NoSuchElementException();
        }
    }
}
