/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;

public class FlowgraphVarExpression extends JBCExpression {
    private ExternalCFGVariable v;
    
    public ExternalCFGVariable getVar() {
        return v;
    }
    
    FlowgraphVarExpression(ExternalCFGVariable v) {
        this.v = v;
    }
}
