/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.*;

class InheritedFieldEnumerator implements Enumeration {
    private JBCClass c;
    private Enumeration fieldList = null;
    
    InheritedFieldEnumerator(JBCClass c) {
        this.c = c;
    }
    
    private void normalize() {
        if (fieldList == null || !fieldList.hasMoreElements()) {
            while (c != null) {
                fieldList = c.getFields();
                c = c.getSuperClass();
                if (fieldList.hasMoreElements()) {
                    return;
                }
            }
            fieldList = null;
        }
    }
    
    public boolean hasMoreElements() {
        normalize();
        return fieldList != null;
    }
    
    public Object nextElement() {
        normalize();
        if (fieldList == null) {
            throw new NoSuchElementException();
        } else {
            return fieldList.nextElement();
        }
    }
}
