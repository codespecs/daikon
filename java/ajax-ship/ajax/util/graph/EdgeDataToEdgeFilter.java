/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.Globals;
import ajax.util.*;
import java.util.*;

class EdgeDataToEdgeFilter extends EnumerationMap {
    EdgeDataToEdgeFilter(Enumeration e) {
        super(e);
    }
    
    protected Object map(Object o) {
        return ((GraphEdgeData)o).getEdge();
    }
}
