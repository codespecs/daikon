/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;
import ajax.jbc.util.ExternalNamedVariable;

class SalamisFlowgraphCallDefNode extends SalamisDefNode implements ExternalCFGFlowgraphInvocationDefNode {
    private ExternalCFGVariable[] params;
    private String functionName;
    private ExternalCFGVariable exceptionVar = new ExternalNamedVariable("exception");
    private SalamisFlowgraph fg;
    
    SalamisFlowgraphCallDefNode(SalamisParser p, String defVar, String functionName, ExternalCFGVariable[] params) {
        super(p, defVar);
        this.functionName = functionName;
        this.params = params;
    }

    public ExternalCFGVariable[] getParameters() {
        return params;
    }
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
    
    public void verify() {
        super.verify();
        getFunctionName();
    }
    
    public String getFunctionName() {
        String result = fg.resolveFlowgraph(functionName);
        
        if (result == null) {
            throw new InvalidFlowgraphError("Flowgraph not found: " + functionName);
        } else {
            return result;
        }
    }
    
    public ExternalCFGVariable[] getDefinedVariables() {
        ExternalCFGVariable[] defVars = { defVar, exceptionVar };
        
        return defVars;
    }
}
