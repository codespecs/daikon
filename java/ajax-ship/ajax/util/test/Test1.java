/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.test;

import ajax.util.*;
import java.util.Enumeration;

public class Test1 {
    public static void main(String[] args) {
        Integer[] objs = new Integer[10000];
        IdentityCompactSet identitySet = new IdentityCompactSet();
        CompactSet set = new CompactSet();
        
        for (int i = 0; i < objs.length; i++) {
            objs[i] = new Integer(i*187714451);
            identitySet.addUnconditionally(objs[i]);
            set.addUnconditionally(objs[i]);
        }
        
        int sum1 = 0;
        int sum2 = 0;

        for (Enumeration e = identitySet.elements(); e.hasMoreElements();) {
            sum1 += ((Integer)e.nextElement()).intValue();
        }
        
        for (Enumeration e = set.elements(); e.hasMoreElements();) {
            sum2 += ((Integer)e.nextElement()).intValue();
        }
        
        System.out.println("Totals: " + sum1 + ", " + sum2);
    }
}
