/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalFG implements ExternalFlowgraph, Serializable {
    private ExternalCFGNode root;
    private ExternalCFGDefNode result;
    private ExternalCFGDefNode exception;
    
    public ExternalFG() {
        this(null, null, null);
    }
    
    public ExternalFG(ExternalCFGNode root) {
        this(root, null, null);
    }
    
    public ExternalFG(ExternalCFGNode root, ExternalCFGDefNode result) {
        this(root, result, null);
    }
    
    public ExternalFG(ExternalCFGNode root, ExternalCFGDefNode result, ExternalCFGDefNode exception) {
        this.root = root;
        this.result = result;
        this.exception = exception;
        
        if (this.root == null) {
            this.root = new ExternalNode();
        }
    }
    
    public ExternalCFGNode getCFGRoot() {
        return root;
    }
    
    public ExternalCFGDefNode getResultDef() {
        return result;
    }
    
    public ExternalCFGDefNode getExceptionDef() {
        return exception;
    }
}
