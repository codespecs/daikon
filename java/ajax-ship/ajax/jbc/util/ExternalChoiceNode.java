/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalChoiceNode extends ExternalDefNode implements ExternalCFGChoiceDefNode, Serializable {
    private ExternalCFGVariable[] vars;
    
    private static final ExternalCFGVariable[] noVars = new ExternalCFGVariable[0];
    
    private void init() {
        vars = noVars;
    }
    
    private void init(ExternalCFGVariable v) {
        ExternalCFGVariable[] vars = { v };
        
        this.vars = vars;
    }
    
    private void init(ExternalCFGVariable[] vars) {
        this.vars = vars;
    }
    
    public ExternalChoiceNode() {
        init();
    }
    
    public ExternalChoiceNode(ExternalCFGVariable[] from) {
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGVariable def, ExternalCFGVariable from) {
        super(def);
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGVariable def, ExternalCFGVariable[] from) {
        super(def);
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGNode successor, ExternalCFGVariable from) {
        super(successor);
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGNode successor, ExternalCFGVariable[] from) {
        super(successor);
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGNode successor, ExternalCFGVariable def, ExternalCFGVariable from) {
        super(successor, def);
        init(from);
    }
    
    public ExternalChoiceNode(ExternalCFGNode successor, ExternalCFGVariable def, ExternalCFGVariable[] from) {
        super(successor, def);
        init(from);
    }
    
    public ExternalCFGVariable[] getChoiceValues() {
        return vars;
    }
}
