/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.Globals;
import ajax.util.CompactSet;
import java.util.*;

public class SingletonTracker extends IndirectionQueryFamily {
    public static final Object MANY = new String("MANY");
    
    public SingletonTracker(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] listeners) {
        super(analyzer, specifiers, listeners);
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return join(o1, o2);
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return intersect(o1, o2);
    }
    
    public static String toString(Object o) {
        if (o == null) {
            return "None";
        } else if (o == MANY) {
            return "Many";
        } else {
            return o.toString();
        }
    }
    
    public static Object intersect(Object o1, Object o2) {
        if (o1 == MANY) {
            return o2;
        } else if (o2 == MANY || o1.equals(o2)) {
            return o1;
        } else {
            return null;
        }
    }
    
    public static Object join(Object o1, Object o2) {
        if (o1.equals(o2)) {
            return o1;
        } else {
            return MANY;
        }
    }
}
