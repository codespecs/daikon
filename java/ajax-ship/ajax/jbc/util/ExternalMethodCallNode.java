/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalMethodCallNode extends ExternalDefNode implements ExternalCFGMethodInvocationDefNode, java.io.Serializable {
    private JBCMethod method;
    private ExternalCFGVariable[] params;
    
    private void init(JBCMethod method, ExternalCFGVariable[] params) {
        this.method = method;
        this.params = params;
    }
    
    public ExternalMethodCallNode(ExternalCFGVariable def, JBCMethod method, ExternalCFGVariable[] params) {
        super(def);
        init(method, params);
    }
    
    public ExternalMethodCallNode(JBCMethod method, ExternalCFGVariable[] params) {
        super();
        init(method, params);
    }
    
    public ExternalMethodCallNode(ExternalCFGNode successor, JBCMethod method, ExternalCFGVariable[] params) {
        super(successor);
        init(method, params);
    }
    
    public ExternalMethodCallNode(ExternalCFGNode successor, ExternalCFGVariable def, JBCMethod method, ExternalCFGVariable[] params) {
        super(successor, def);
        init(method, params);
    }
    
    public ExternalCFGVariable[] getParameters() {
        return params;
    }
    
    public JBCMethod getMethod() {
        return method;
    }
    
    public String toString() {
        return "external call to " + method;
    }
}
