/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.test;

import java.util.*;

public class PolyContainerTest {
    public static void main(String[] args) {
        Vector list = new Vector();
        Vector list2 = new Vector();
        Hashtable table = new Hashtable();
        
        for (int i = 0; i < 100; i++) {
            Integer v1 = new Integer(i);
            Float v2 = new Float(i);
            
            list.addElement(v1);
            list2.addElement(v2);
            table.put(v1, v2);
        }
        
        int sum = 0;
        
        for (Enumeration e = list.elements(); e.hasMoreElements();) {
            sum += ((Integer)e.nextElement()).intValue();
        }
    }
}
