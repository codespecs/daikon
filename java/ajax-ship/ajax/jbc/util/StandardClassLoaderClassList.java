/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import java.util.*;

class StandardClassLoaderClassList implements Enumeration {
    private int nextReader = 0;
    private ClassReader[] readers;
    private Enumeration curList = null;
    private boolean atNextElement = false;
    
    StandardClassLoaderClassList(ClassReader[] readers) {
        this.readers = readers;
    }
    
    public boolean hasMoreElements() {
        if (atNextElement) {        
            return true;
        } else {
            if (curList != null && curList.hasMoreElements()) {
                atNextElement = true;
                return true;
            } else {
                while (nextReader < readers.length) {
                    curList = readers[nextReader].getClassNames();
                    nextReader++;

                    if (curList.hasMoreElements()) {
                        atNextElement = true;
                        return true;
                    }
                }

                curList = null;
                return false;
            }
        }
    }
            
    public Object nextElement() {    
        if (atNextElement || hasMoreElements()) {
            atNextElement = false;
            return curList.nextElement();
        } else {
            throw new NoSuchElementException();
        }
    }
}
