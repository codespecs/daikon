/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.Globals;
import ajax.jbc.*;
import java.util.Enumeration;
import ajax.util.*;

public class ExternalNode implements ExternalCFGNode, java.io.Serializable {
    private Object successors;
    
    public ExternalNode() {
        successors = null;
    }
    
    public void setSuccessor(ExternalCFGNode successor) {
        if (Globals.debug && this.successors != null) {
            Globals.nonlocalError("Cannot reset successors!");
        }
        
        successors = successor;
    }
        
    public void setSuccessors(ExternalCFGNode[] successors) {
        if (Globals.debug && this.successors != null) {
            Globals.nonlocalError("Cannot reset successors!");
        }
        
        this.successors = successors;
    }
        
    public ExternalNode(ExternalCFGNode successor) {
        successors = successor;
    }
    
    public ExternalNode(ExternalCFGNode[] successors) {
        this.successors = successors;
    }
    
    public Enumeration getSuccessors() {
        if (successors == null) {
            return new EmptyEnumerator();
        } else if (successors instanceof ExternalCFGNode) {
            return new SingletonEnumerator(successors);
        } else {
            return new ArrayEnumerator((ExternalCFGNode[])successors);
        }
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
