/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.Globals;
import ajax.jbc.*;
import java.util.Hashtable;

class SalamisFieldDefNode extends SalamisDefNode implements ExternalCFGFieldDefNode {
    private ExternalCFGVariable obj;
    private String fieldName;
    private SalamisFlowgraph fg;
    
    SalamisFieldDefNode(SalamisParser p, String defVar, ExternalCFGVariable obj, String fieldName) {
        super(p, defVar);
        this.fieldName = fieldName;
        this.obj = obj;
    }

    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public void verify() {
        super.verify();
        getField();
    }
    
    public JBCField getField() {
        JBCField found = fg.resolveField(fieldName);
        
        if (found == null) {
            throw new InvalidFlowgraphError("Field not found: " + fieldName);
        }
        
        return found;
    }
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
}
