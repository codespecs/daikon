/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util.test;

import ajax.analyzer.util.*;
import java.util.*;

public class Test1 {
    static Integer[] objs = new Integer[64];
    static Random r = new Random(1000);
    
    public static int random(int n) {
        return Math.abs(r.nextInt()) % n;
    }
    
    private static String binString(long v) {
        StringBuffer b = new StringBuffer();
        
        for (int i = 0; i < 64; i++) {
            if ((v & (1 << i)) != 0) {
                b.append(i + ", ");
            }
        }
        
        return b.toString();
    }
    
    private static void test1(int K) {
        for (int i = 0; i < objs.length; i++) {
            objs[i] = new Integer(i);
        }
        
        Object[] o2s = new Object[100];
        long[] vs = new long[100];
        
        for (int i = 0; i < o2s.length; i++) {
            int iters = random(50);
            long v = 0;
            Object o = null;
            
            for (int j = 0; j < iters; j++) {
                int r = random(objs.length);
                
                o = BoundedSetTracker.join(o, objs[r], K);
                v = v | (1L << r);
            }
            
            o2s[i] = o;
            vs[i] = v;
        }
        
        for (int i = 0; i < 1000; i++) {
            int iters = random(10) + 1;
            long v = -1;
            Object o = null;
            StringBuffer b = new StringBuffer();
            boolean hasOverflows = false;
            
            for (int j = 0; j < iters; j++) {
                int r = random(o2s.length);
                
                if (j == 0) {
                    o = o2s[r];
                } else {
                    o = BoundedSetTracker.intersect(o, o2s[r], K);
                }
                v = v & vs[r];
                b.append(vs[r] + " (" + (o2s[r] == null ? "null" : o2s[r].getClass().toString()) + ": " + o2s[r] + "), ");
                if (o2s[r] instanceof OverflowSet) {
                    hasOverflows = true;
                }
            }
            
            long v2 = 0;
            
            for (Enumeration e = BoundedSetTracker.enumerateIntermediate(o); e.hasMoreElements();) {
                int x = ((Integer)e.nextElement()).intValue();
                
                v2 = v2 | (1L << x);
            }
            
            if ((!hasOverflows && v2 != v)
                || (o instanceof OverflowSet && (~v & v2) != 0)) {
                System.err.println("Mismatched: " + (o == null ? "(null)" : o.getClass().toString()) + ": " + o + ", value = "
                    + v + " (" + binString(v) + ")"
                    + "\n" + b.toString());
            }
        }
    }
    
    public static void main(String[] args) {
        test1(100);
        test1(30);
    }
}
