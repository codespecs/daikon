/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalFlowgraphCallNode extends ExternalDefNode implements ExternalCFGFlowgraphInvocationDefNode, java.io.Serializable {
    private String fg;
    private ExternalCFGVariable[] params;
    
    private void init(String fg, ExternalCFGVariable[] params) {
        this.fg = fg;
        this.params = params;
    }
    
    public ExternalFlowgraphCallNode(String fg, ExternalCFGVariable[] params) {
        init(fg, params);
    }
    
    public ExternalFlowgraphCallNode(ExternalCFGNode successor, String fg, ExternalCFGVariable[] params) {
        super(successor);
        init(fg, params);
    }
    
    public ExternalFlowgraphCallNode(ExternalCFGNode successor, ExternalCFGVariable def, String fg, ExternalCFGVariable[] params) {
        super(successor, def);
        init(fg, params);
    }
    
    public ExternalCFGVariable[] getParameters() {
        return params;
    }
    
    public String getFunctionName() {
        return fg;
    }
    
    public String toString() {
        return "external call to " + fg;
    }
}
