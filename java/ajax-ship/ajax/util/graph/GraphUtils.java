/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.util.*;
import java.util.*;

public class GraphUtils {
    public static CompactSet complementNodes(GraphView g, CompactSet nodes) {
        CompactSet result = new CompactSet();
        
        for (Enumeration e = g.getNodes(); e.hasMoreElements();) {
            GraphNode n = (GraphNode)e.nextElement();
            
            if (nodes.get(n) == null) {
                result.addUnconditionally(n);
            }
        }
        
        return result;
    }
}
