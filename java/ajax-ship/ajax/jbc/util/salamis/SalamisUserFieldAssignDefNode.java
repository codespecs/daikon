/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;

class SalamisUserFieldAssignDefNode extends SalamisDefNode implements ExternalCFGUserFieldAssignmentDefNode {
    private ExternalCFGVariable obj;
    private ExternalCFGVariable value;
    private String fieldName;
    private SalamisFlowgraph fg;
    
    SalamisUserFieldAssignDefNode(SalamisParser p, String defVar,
        ExternalCFGVariable obj, String fieldName, ExternalCFGVariable value) {
        super(p, defVar);
        this.fieldName = fieldName;
        this.obj = obj;
        this.value = value;
    }

    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public UserField getField() {
        return fg.resolveUserField(fieldName, obj == null);
    }
    
    public ExternalCFGVariable getValue() {
        return value;
    }

    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
}
