/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalFieldNode extends ExternalDefNode implements ExternalCFGFieldDefNode, Serializable {
    private JBCField f;
    private ExternalCFGVariable obj;
    
    private void init(ExternalCFGVariable obj, JBCField f) {
        this.obj = obj;
        this.f = f;
    }
    
    public ExternalFieldNode(ExternalCFGVariable obj, JBCField f) {
        super();
        init(obj, f);
    }
    
    public ExternalFieldNode(ExternalCFGVariable def, ExternalCFGVariable obj, JBCField f) {
        super(def);
        init(obj, f);
    }
    
    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public JBCField getField() {
        return f;
    }
}
