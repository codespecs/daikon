/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.util.graph;

import ajax.util.IdentityManager;

public class StdGraphEdge implements GraphEdge {
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
