/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;

public class ReachabilityTracker extends IndirectionQueryFamily {
    public ReachabilityTracker(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] listeners) {
        super(analyzer, specifiers, listeners);
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return o1;
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return o1;
    }
    
    public static String toString(Object o) {
        if (o == null) {
            return "None";
        } else {
            return "Something";
        }
    }
}
