/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.io.Serializable;
import java.util.*;
import ajax.util.ArrayEnumerator;
import ajax.Globals;

class SalamisGotoNode extends SalamisNode {
    private ExternalCFGNode[] nodes = null;
    private String[] targets;
    
    SalamisGotoNode(String[] targets) {
        this.targets = targets;
    }
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        nodes = new ExternalCFGNode[targets.length];
        
        for (int i = 0; i < nodes.length; i++) {
            ExternalCFGNode target = (ExternalCFGNode)labels.get(targets[i]);
            
            if (target == null) {
                Globals.userError("Invalid Salamis target node in goto: " + targets[i]);
            }
            
            nodes[i] = target;
        }
        
        targets = null;
    }
    
    public Enumeration getSuccessors() {
        return new ArrayEnumerator(nodes);
    }
}
