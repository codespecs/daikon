/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalUserFieldNode extends ExternalDefNode implements ExternalCFGUserFieldDefNode, Serializable {
    private UserField f;
    private ExternalCFGVariable obj;
    
    private void init(ExternalCFGVariable obj, UserField f) {
        this.obj = obj;
        this.f = f;
    }
    
    public ExternalUserFieldNode(ExternalCFGVariable obj, UserField f) {
        super();
        init(obj, f);
    }
    
    public ExternalUserFieldNode(ExternalCFGVariable def, ExternalCFGVariable obj, UserField f) {
        super(def);
        init(obj, f);
    }
    
    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public UserField getField() {
        return f;
    }
}
