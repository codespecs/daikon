/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalDefNode extends ExternalNode implements ExternalCFGDefNode, java.io.Serializable {
    private ExternalCFGVariable[] defVar;
    
    private static final ExternalCFGVariable[] noVar =
        { new ExternalAnonymousVariable() };
    
    private void init() {
        defVar = noVar;
    }
    
    private void init(ExternalCFGVariable v) {
        ExternalCFGVariable[] var = { v };
        
        defVar = var;
    }

    private void init(ExternalCFGVariable[] vs) {
        defVar = vs;
    }
    
    public ExternalDefNode() {
        init();
    }
    
    public ExternalDefNode(ExternalCFGVariable def) {
        init(def);
    }
    
    public ExternalDefNode(ExternalCFGNode successor) {
        super(successor);
        init();
    }
    
    public ExternalDefNode(ExternalCFGNode successor, ExternalCFGVariable def) {
        super(successor);
        init(def);
    }
    
    public ExternalCFGVariable[] getDefinedVariables() {
        return defVar;
    }
}
