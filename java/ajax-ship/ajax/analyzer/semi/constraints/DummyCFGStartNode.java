/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.ExternalCFGNode;
import ajax.util.SingletonEnumerator;
import java.util.Enumeration;

/**
This class serves two functions. First, it is used to create an entry node
to an ExternalFlowgraph that is guaranteed to have no predecessors.
Also, the result computed by an ExternalFlowgraph is returned along the
flow path from the result node to this start node. That is, we pretend that
the result node uses this start node as a dataflow source, and pass the
return value backwards along that path.
*/
class DummyCFGStartNode implements ExternalCFGNode {
    private ExternalCFGNode successor;
    
    DummyCFGStartNode(ExternalCFGNode successor) {
        this.successor = successor;
    }
    
    public Enumeration getSuccessors() {
        return new SingletonEnumerator(successor);
    }
}
