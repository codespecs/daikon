/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.util.*;
import java.util.*;

public class OverflowSet extends CompactSetBase {
    public OverflowSet(int initialSize, Enumeration initialElements) {
        super(initialSize, initialElements);
    }
    
    public OverflowSet() {
        super();
    }
    
    protected boolean checkEqual(Object o1, Object o2) {
        return o1 == o2 || o1.equals(o2);
    }
    
    protected int getHash(Object o) {
        return o.hashCode()*1299827;
    }
    
    public boolean equals(Object o) {
        return o instanceof OverflowSet;
    }
    
    public int hashCode() {
        return 3180101;
    }
}
