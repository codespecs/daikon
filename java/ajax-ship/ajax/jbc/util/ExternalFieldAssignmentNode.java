/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.io.Serializable;

public class ExternalFieldAssignmentNode extends ExternalDefNode implements ExternalCFGFieldAssignmentDefNode, Serializable {
    private JBCField f;
    private ExternalCFGVariable obj;
    private ExternalCFGVariable value;
    
    private void init(ExternalCFGVariable obj, JBCField f, ExternalCFGVariable value) {
        this.obj = obj;
        this.f = f;
        this.value = value;
    }
    
    public ExternalFieldAssignmentNode(JBCField f, ExternalCFGVariable value) {
        super();
        init(null, f, value);
    }
    
    public ExternalFieldAssignmentNode(ExternalCFGVariable obj, JBCField f, ExternalCFGVariable value) {
        super();
        init(obj, f, value);
    }
    
    public ExternalFieldAssignmentNode(ExternalCFGVariable def, ExternalCFGVariable obj, JBCField f, ExternalCFGVariable value) {
        super(def);
        init(obj, f, value);
    }
    
    public ExternalCFGVariable getObject() {
        return obj;
    }
    
    public JBCField getField() {
        return f;
    }
    
    public ExternalCFGVariable getValue() {
        return value;
    }
}
