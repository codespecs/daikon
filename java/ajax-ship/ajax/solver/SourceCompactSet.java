/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.Globals;
import ajax.util.*;
import java.util.*;

class SourceCompactSet extends CompactSetBase {
    private static Enumeration enumeratePair(InstantiationRecord r1, InstantiationRecord r2) {
        Object[] objs = { r1, r2 };
        
        return new ArrayEnumerator(objs);
    }
    
    SourceCompactSet(InstantiationRecord r1, InstantiationRecord r2) {
        super(2, enumeratePair(r1, r2));
    }
    
    SourceCompactSet() {
        super();
    }
    
    protected boolean checkEqual(Object o1, Object o2) {
        InstantiationRecord r1 = (InstantiationRecord)o1;
        InstantiationRecord r2 = (InstantiationRecord)o2;
        
        return r1.getSourceHead() == r2.getSourceHead()
            && r1.label.equals(r2.label);
    }
    
    protected int getHash(Object o) {
        InstantiationRecord r = (InstantiationRecord)o;
        
        return r.getSourceHead().hashCode()*10400101 + r.label.hashCode()*10047;
    }
}
