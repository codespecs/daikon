/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

public class ObjectQueue {
    private Vector queue = null;
    private Enumeration objects = null;
    private boolean done = false;
    
    public ObjectQueue() {
    }
    
/**
This method never blocks.
*/
    public synchronized void addObject(Object o) {
        if (queue == null) {
            queue = new Vector();
        }
        
        queue.addElement(o);
        notify();
    }
    
    public synchronized void terminate() {
        done = true;
        notify();
    }
    
/**
This method may block.
*/
    public synchronized Object removeObject() throws NoSuchElementException {
        while (queue == null && objects == null && !done) {
            try {
                wait();
            } catch (InterruptedException ex) {
            }
        }
        
        if (done) {
            throw new NoSuchElementException();
        }
        
        if (objects == null) {
            objects = queue.elements();
            queue = null;
        }
        
        Object result = objects.nextElement();
        
        if (!objects.hasMoreElements()) {
            objects = null;
        }
        
        notify();
        
        return result;
    }
}
