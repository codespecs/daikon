/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;
import ajax.Globals;
import ajax.jbc.util.*;

class SalamisMethodCallDefNode extends SalamisDefNode implements ExternalCFGMethodInvocationDefNode {
    private ExternalCFGVariable[] params;
    private String methodName;
    private String signature;
    private SalamisFlowgraph fg;
    private ExternalCFGVariable exceptionVar = new ExternalNamedVariable("exception");
    
    SalamisMethodCallDefNode(SalamisParser p, String defVar, String methodName, ExternalCFGVariable[] params, String signature) {
        super(p, defVar);
        this.methodName = methodName;
        this.signature = signature;
        this.params = params;
    }

    public ExternalCFGVariable[] getParameters() {
        return params;
    }
    
    public void verify() {
        super.verify();
        getMethod();
    }
    
    public JBCMethod getMethod() {
        try {
            return fg.resolveMethod(methodName, signature, params.length);
        } catch (AmbiguousMethodException ex) {
            throw new InvalidFlowgraphError(ex.getMessage());
        } catch (MissingMethodException ex) {
            throw new InvalidFlowgraphError(ex.getMessage());
        } catch (UnresolvedClassException ex) {
            throw new InvalidFlowgraphError(ex.getMessage());
        }
    } 
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
    
    public ExternalCFGVariable[] getDefinedVariables() {
        ExternalCFGVariable[] defVars = { defVar, exceptionVar };
        
        return defVars;
    }
    
    public String toString() {
        if (signature != null) {
            return "external call to " + methodName + " [" + signature + "]";
        } else {
            return "external call to " + methodName;
        }
    }
}
