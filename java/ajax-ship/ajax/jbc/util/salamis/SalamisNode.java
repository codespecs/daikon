/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.io.Serializable;
import java.util.*;
import ajax.util.*;

abstract class SalamisNode implements ExternalCFGNode, Serializable {
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
    }
    
    public void verify() throws InvalidFlowgraphError {
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
