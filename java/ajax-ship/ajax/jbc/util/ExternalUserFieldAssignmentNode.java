/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalUserFieldAssignmentNode extends ExternalDefNode implements ExternalCFGUserFieldAssignmentDefNode, Serializable {
    private UserField f;
    private ExternalCFGVariable obj;
    private ExternalCFGVariable value;
    
    private void init(ExternalCFGVariable obj, UserField f, ExternalCFGVariable value) {
        this.obj = obj;
        this.f = f;
        this.value = value;
    }
    
    public ExternalUserFieldAssignmentNode(UserField f, ExternalCFGVariable value) {
        super();
        init(null, f, value);
    }
    
    public ExternalUserFieldAssignmentNode(ExternalCFGVariable obj, UserField f, ExternalCFGVariable value) {
        super();
        init(obj, f, value);
    }
    
    public ExternalUserFieldAssignmentNode(ExternalCFGVariable def, ExternalCFGVariable obj, UserField f, ExternalCFGVariable value) {
        super(def);
        init(obj, f, value);
    }
    
    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public UserField getField() {
        return f;
    }
    
    public ExternalCFGVariable getValue() {
        return value;
    }
}
