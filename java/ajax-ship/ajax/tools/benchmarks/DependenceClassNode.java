/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.util.graph.*;
import ajax.jbc.*;

class DependenceClassNode implements GraphNode {
    private JBCClass c;
    
    DependenceClassNode(JBCClass c) {
        this.c = c;
    }
    
    JBCClass getClassInfo() {
        return c;
    }
    
    public String toString() {
        return c.toString();
    }
}
