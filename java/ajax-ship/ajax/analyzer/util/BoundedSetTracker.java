/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.Globals;
import ajax.util.*;
import java.util.*;

/**
An intermediate object for the bounded set tracker has one of four forms:
    null:
        the identity datum
    object not an instance of CompactSetBase:
        the singleton object
    a CompactSet with at least 2 elements and at most K elements:
        exactly the given set of objects
    an OverflowSet with at least 0 elements and at most K+1 elements:
        a set containing at least the given set of objects
*/
public class BoundedSetTracker extends IndirectionQueryFamily {
    private int K;
    private boolean keepOverflowData = false;
    
    private static final OverflowSet overflows = new OverflowSet();
    
    public BoundedSetTracker(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] listeners, int K) {
        super(analyzer, specifiers, listeners);
        
        this.K = K;
        
        if (Globals.debug && K < 0) {
            Globals.nonlocalError("Invalid K limit: " + K);
        }
    }
    
    public void setKeepOverflowData(boolean k) {
        keepOverflowData = k;
    }
    
    public static Enumeration enumerateIntermediate(Object o) {
        return UnboundedSetTracker.enumerateIntermediate(o);
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return join(o1, o2, K, keepOverflowData);
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return intersect(o1, o2, K, keepOverflowData);
    }
    
    public static String toString(Object o) {
        if (o == null) {
            return "None";
        } else if (o instanceof OverflowSet) {
            return "MORE THAN " + o.toString();
        } else {
            return o.toString();
        }
    }
    
    public static Object intersect(Object o1, Object o2, int K) {
        return intersect(o1, o2, K, false);
    }
    
    public static Object intersect(Object o1, Object o2, int K, boolean keepOverflowData) {
        if (o1 == null || o2 == null) {
            return null;
        }
        
        if (o1 instanceof OverflowSet) {
            if (keepOverflowData && o2 instanceof OverflowSet) {
                // Find the elements that are definitely in the overflow set
                int elemCount = 0;
                OverflowSet o1Set = (OverflowSet)o1;
                OverflowSet o2Set = (OverflowSet)o2;
                
                for (Enumeration e = o2Set.elements(); e.hasMoreElements();) {
                    if (o1Set.get(e.nextElement()) != null) {
                        elemCount++;
                    }
                }
                
                if (elemCount == o1Set.size()) {
                    return o1;
                } else if (elemCount == o2Set.size()) {
                    return o2;
                } else {
                    OverflowSet set = new OverflowSet();
                    
                    for (Enumeration e = o2Set.elements(); e.hasMoreElements();) {
                        Object o = e.nextElement();
                        
                        if (o1Set.get(o) != null) {
                            set.addUnconditionally(o);
                        }
                    }
                
                    return set;
                }
            } else {
                return o2;
            }
        } else if (o2 instanceof OverflowSet) {
            return o1;
        }
        
        boolean o2IsSet = o2 instanceof CompactSet;

        if (!(o1 instanceof CompactSet)) {
            if (o2IsSet) {
                Object otmp = o1;
                o1 = o2;
                o2 = otmp;
                
                o2IsSet = false;
            } else {
                if (o1.equals(o2)) {
                    return o1;
                } else {
                    return null;
                }
            }
        }
        
        CompactSet o1Set = (CompactSet)o1;
        
        if (!o2IsSet) {
            if (o1Set.get(o2) != null) {
                return o2;
            } else {
                return null;
            }
        }
        
        int newSize = 0;
        CompactSet o2Set = (CompactSet)o2;
        Object singleton = null;
        
        for (Enumeration e = o2Set.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o1Set.get(o) != null) {
                newSize++;
                singleton = o;
            }
        }
        
        if (newSize == o1Set.size()) {
            return o1;
        } else if (newSize == o2Set.size()) {
            return o2;
        } else if (newSize == 1) {
            return singleton;
        } else if (newSize == 0) {
            return null;
        }
        
        CompactSet set = new CompactSet();
                
        for (Enumeration e = o2Set.elements(); e.hasMoreElements();) {
            Object o = e.nextElement();
            
            if (o1Set.get(o) != null) {
                set.addUnconditionally(o);
            }
        }
        
        return set;
    }

    public static Object join(Object o1, Object o2, int K) {
        return join(o1, o2, K, false);
    }
    
    public static Object join(Object o1, Object o2, int K, boolean keepOverflowData) {
        if (o1 == null) {
            return o2;
        } else if (o2 == null) {
            return o1;
        }
        
        if (o1 instanceof OverflowSet) {
            return o1;
        } else if (o2 instanceof OverflowSet) {
            return o2;
        }
        
        boolean o2IsSet = o2 instanceof CompactSet;

        if (!(o1 instanceof CompactSet)) {
            if (o2IsSet) {
                Object otmp = o1;
                o1 = o2;
                o2 = otmp;
                
                o2IsSet = false;
            } else {
                if (o1.equals(o2)) {
                    return o1;
                } else if (!keepOverflowData && K < 2) {
                    return overflows;
                } else {
                    CompactSetBase set = K < 2 ? (CompactSetBase)(new OverflowSet()) : (CompactSetBase)(new CompactSet());
                    
                    set.addUnconditionally(o1);
                    if (K >= 1) {
                        set.addUnconditionally(o2);
                    }
                    return set;
                }
            }
        }
        
        CompactSet o1Set = (CompactSet)o1;
        
        if (!o2IsSet) {
            if (o1Set.get(o2) != null) {
                return o1Set;
            } else if (!keepOverflowData && o1Set.size() >= K) {
                return overflows;
            } else {
                CompactSetBase set = o1Set.size() >= K ? ((CompactSetBase)new OverflowSet()) : ((CompactSetBase)new CompactSet());
                
                for (Enumeration e = o1Set.elements(); e.hasMoreElements();) {
                    set.addUnconditionally(e.nextElement());
                }
                set.addUnconditionally(o2);
                return set;
            }
        }
        
        int newSize = o1Set.size();
        CompactSet o2Set = (CompactSet)o2;
        
        for (Enumeration e = o2Set.elements(); e.hasMoreElements();) {
            if (o1Set.get(e.nextElement()) == null) {
                newSize++;
            }
        }
        
        if (newSize == o1Set.size()) {
            return o1;
        } else if (newSize == o2Set.size()) {
            return o2;
        }
        
        if (!keepOverflowData && newSize > K) {
            return overflows;
        } else {
            CompactSetBase set = newSize > K ? ((CompactSetBase)new OverflowSet()) : ((CompactSetBase)new CompactSet());
                    
            newSize = 0;
            for (Enumeration e = o1Set.elements(); e.hasMoreElements() && newSize <= K;) {
                set.addUnconditionally(e.nextElement());
                newSize++;
            }
            for (Enumeration e = o2Set.elements(); e.hasMoreElements() && newSize <= K;) {
                Object o = e.nextElement();
                
                if (set.get(o) == null) {
                    set.addUnconditionally(o);
                    newSize++;
                }
            }
            
            return set;
        }
    }
}
