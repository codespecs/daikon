/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;
import java.util.Hashtable;

class SalamisUserFieldDefNode extends SalamisDefNode implements ExternalCFGUserFieldDefNode {
    private ExternalCFGVariable obj;
    private String fieldName;
    private SalamisFlowgraph fg;
    
    SalamisUserFieldDefNode(SalamisParser p, String defVar, ExternalCFGVariable obj, String fieldName) {
        super(p, defVar);
        this.fieldName = fieldName;
        this.obj = obj;
    }

    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public UserField getField() {
        return fg.resolveUserField(fieldName, obj == null);
    }
    
    void resolve(Hashtable labels, SalamisFlowgraph fg) {
        this.fg = fg;
    }
}
