/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.Globals;
import ajax.jbc.*;
import java.util.Hashtable;

class SalamisFieldAssignDefNode extends SalamisDefNode implements ExternalCFGFieldAssignmentDefNode {
    private ExternalCFGVariable obj;
    private ExternalCFGVariable value;
    private String fieldName;
    private SalamisFlowgraph fg;
    
    SalamisFieldAssignDefNode(SalamisParser p, String defVar,
        ExternalCFGVariable obj, String fieldName, ExternalCFGVariable value) {
        super(p, defVar);
        this.fieldName = fieldName;
        this.obj = obj;
        this.value = value;
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
    
    public ExternalCFGVariable getValue() {
        return value;
    }

    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
}
