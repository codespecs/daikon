/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.Globals;
import ajax.util.*;
import java.util.*;

public class UnboundedSetTracker extends IndirectionQueryFamily {
    private WeakSet setCache = new WeakSet();
    
    public UnboundedSetTracker(Analyzer analyzer, DatumSpecifier[] specifiers, ResultListener[] listeners) {
        super(analyzer, specifiers, listeners);
    }
    
    public Object joinIntermediates(Object o1, Object o2) {
        return join(setCache, o1, o2);
    }
    
    public Object intersectIntermediates(Object o1, Object o2) {
        return intersect(setCache, o1, o2);
    }
    
    public static Enumeration enumerateIntermediate(Object o) {
        if (o == null) {
            return EmptyEnumerator.get();
        } else if (o instanceof CompactSetBase) {
            return ((CompactSetBase)o).elements();
        } else {
            return new SingletonEnumerator(o);
        }
    }
    
    public static String toString(Object o) {
        if (o == null) {
            return "None";
        } else {
            return o.toString();
        }
    }
    
    public static Object intersect(WeakSet setCache, Object o1, Object o2) {
        HashableCompactSet items1;
        HashableCompactSet items2;
        
        if (o1 instanceof HashableCompactSet) {
            items1 = (HashableCompactSet)o1;
        } else {
            items1 = null;
        }

        if (o2 instanceof HashableCompactSet) {
            items2 = (HashableCompactSet)o2;
        } else {
            items2 = null;
        }
        
        int items1Size = items1 != null ? items1.size() : 1;
        int items2Size = items2 != null ? items2.size() : 1;
        
        if (items1Size < items2Size) {
            HashableCompactSet tmp = items2;
            
            items2 = items1;
            items1 = tmp;
            
            Object otmp = o2;
            
            o2 = o1;
            o1 = otmp;
            
            int itmp = items2Size;
            
            items2Size = items1Size;
            items1Size = itmp;
        }
        
        int newSize = 0;
        Object singleton = null;
        
        if (items2 == null) {
            if (items1 == null ? o1.equals(o2) : items1.get(o2) != null) {
                newSize++;
                singleton = o2;
            }
        } else {
            for (Enumeration e = items2.elements(); e.hasMoreElements();) {
                Object obj = e.nextElement();
                
                if (items1.get(obj) != null) {
                    newSize++;
                    singleton = obj;
                }
            }
        }
        
        if (newSize == items1Size) {
            return o1;
        } else if (newSize == items2Size) {
            return o2;
        } else if (newSize == 0) {
            return null;
        } else if (newSize == 1) {
            return singleton;
        }
        
        HashableCompactSet result = new HashableCompactSet();
            
        if (items2 == null) {
            result.addUnconditionally(o2);
        } else {
            for (Enumeration e = items2.elements(); e.hasMoreElements();) {
                Object obj = e.nextElement();
                    
                if (items1.get(obj) != null) {
                    result.addUnconditionally(obj);
                }
            }
        }
        
        return setCache.add(result);
    }
    
    public static Object join(WeakSet setCache, Object o1, Object o2) {
        HashableCompactSet items1;
        HashableCompactSet items2;
        
        if (o1 instanceof HashableCompactSet) {
            items1 = (HashableCompactSet)o1;
        } else {
            items1 = null;
        }

        if (o2 instanceof HashableCompactSet) {
            items2 = (HashableCompactSet)o2;
        } else {
            items2 = null;
        }
        
        int items1Size = items1 != null ? items1.size() : 1;
        int items2Size = items2 != null ? items2.size() : 1;
        
        if (items1Size < items2Size) {
            HashableCompactSet tmp = items2;
            
            items2 = items1;
            items1 = tmp;
            
            Object otmp = o2;
            
            o2 = o1;
            o1 = otmp;
            
            int itmp = items2Size;
            
            items2Size = items1Size;
            items1Size = itmp;
        }
        
        int newSize = items1Size;
        
        if (items2 == null) {
            if (items1 == null ? !o1.equals(o2) : items1.get(o2) == null) {
                newSize++;
            }
        } else {
            for (Enumeration e = items2.elements(); e.hasMoreElements();) {
                if (items1.get(e.nextElement()) == null) {
                    newSize++;
                }
            }
        }
        
        if (newSize == items1Size) {
            return o1;
        } else {
            HashableCompactSet result;
            
            if (items1 != null) {
                result = (HashableCompactSet)items1.clone();
            } else {
                result = new HashableCompactSet();
                result.addUnconditionally(o1);
            }
            
            if (items2 == null) {
                result.addUnconditionally(o2);
            } else {
                for (Enumeration e = items2.elements(); e.hasMoreElements();) {
                    result.add(e.nextElement());
                }
            }
            
            return setCache.add(result);
        }
    }
}
