/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util;

import java.util.*;

/**
This class maintains a mapping between keys and values based
in the identity an object. Unlike conventional maps, it does not guarantee
to retain a mapping if all other references to the key are removed.
*/
public class IdentityAssociation {
    private IdentityHashtable table = new IdentityHashtable();
    
    public void put(Object key, Object value) {
        table.put(key, value);
    }
    
    public Object get(Object key) {
        return table.get(key);
    }
}
