/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;
import ajax.util.IdentityManager;

public class DotGraphEdge implements GraphEdge {
    private Hashtable attributes = new Hashtable(3);
    
    public DotGraphEdge() {
    }
    
    public void setAttribute(String attr, String value) {
        attributes.put(attr, value);
    }
    
    public String getAttribute(String attr) {
        return (String)attributes.get(attr);
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
