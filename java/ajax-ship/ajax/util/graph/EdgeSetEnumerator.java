/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import java.util.*;
import ajax.util.*;

class EdgeSetEnumerator implements Enumeration {
    private Enumeration e;
    
    EdgeSetEnumerator(Enumeration e) {
        this.e = e;
    }
    
    public boolean hasMoreElements() {
        return e.hasMoreElements();
    }
    
    public Object nextElement() {
        return ((GraphEdgeData)e.nextElement()).getEdge();
    }
}
