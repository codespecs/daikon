/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import ajax.Globals;
import java.util.Enumeration;

public class CompactSet extends CompactSetBase {
    public CompactSet(int initialSize, Enumeration initialElements) {
        super(initialSize, initialElements);
    }
    
    public CompactSet() {
        super();
    }
    
    protected boolean checkEqual(Object o1, Object o2) {
        return o1 == o2 || o1.equals(o2);
    }
    
    protected int getHash(Object o) {
        return o.hashCode()*1299827;
    }
}
