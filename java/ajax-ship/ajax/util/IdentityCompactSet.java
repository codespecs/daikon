/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.util.Enumeration;

public class IdentityCompactSet extends CompactSetBase {
    public IdentityCompactSet(int initialSize, Enumeration initialElements) {
        super(initialSize, initialElements);
    }
    
    public IdentityCompactSet() {
        super();
    }
    
    protected boolean checkEqual(Object o1, Object o2) {
        return o1 == o2;
    }
    
    protected int getHash(Object o) {
        return IdentityManager.getIdentityHashCode(o);
    }
}
