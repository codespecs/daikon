/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.test;

import ajax.util.*;
import java.util.Enumeration;

public class Test2 {
    public static void main(String[] args) {
        Integer[] objs = new Integer[10000];
        CompactSet set = new CompactSet();
        boolean[] found = new boolean[objs.length];
        
        for (int i = 0; i < objs.length; i++) {
            objs[i] = new Integer(i*1000);
            set.addUnconditionally(objs[i]);
        }
        
        for (Enumeration e = set.elements(); e.hasMoreElements();) {
            int i = ((Integer)e.nextElement()).intValue();
            
            if (i % 1000 == 0) {
                found[i/1000] = true;
            }
            
            set.addUnconditionally(new Integer(i*6 + 777));
        }
        
        for (int i = 0; i < found.length; i++) {
            if (!found[i]) {
                System.err.println("Missing value: " + i);
            }
        }
        
        for (Enumeration e = set.elements(); e.hasMoreElements();) {
            set.remove(e.nextElement());
        }
        
        if (set.size() != 0) {
            System.err.println("Invalid set size: " + set.size());
        }
    }
}
