/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.util.*;

class CompactSetEnumerator implements Enumeration {
    private Object[] items;
    private Object[][] overflows;
    private Object[] list = null;
    private Object next = null;
    private int itemIndex = 0;
    private int overflowIndex = 0;
    private int listIndex = 0;
    
    CompactSetEnumerator(Object[] items, Object[][] overflows) {
        this.items = items;
        this.overflows = overflows;
    }
    
    private void advanceToNextElement() {
        if (next == null) {
            if (items != null) {
                int itemsLen = items.length;
                
                while (itemIndex < itemsLen) {
                    Object n = items[itemIndex];
                    
                    itemIndex++;
                        
                    if (n != null) {
                        next = n;
                        return;
                    }
                }
                
                items = null;
            }
            
            if (list != null) {
                if (listIndex < list.length) {
                    next = list[listIndex];
                    listIndex++;
                    return;
                } else {
                    list = null;
                }
            }
            
            if (overflows != null) {
                int overflowsLen = overflows.length;
                
                while (overflowIndex < overflowsLen) {
                    Object[] l = overflows[overflowIndex];
                    
                    overflowIndex++;
                    
                    if (l != null) {
                        list = l;
                        next = l[0];
                        listIndex = 1;
                        return;
                    }
                }
                
                overflows = null;
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
