/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;

public class ExternalNewObjectNode extends ExternalDefNode implements ExternalCFGNewObjectDefNode, java.io.Serializable {
    private JBCClass c;
    
    public ExternalNewObjectNode(JBCClass c) {
        this.c = c;
    }
    
    public ExternalNewObjectNode(ExternalCFGNode successor, JBCClass c) {
        super(successor);
        this.c = c;
    }
    
    public ExternalNewObjectNode(ExternalCFGVariable def, JBCClass c) {
        super(def);
        this.c = c;
    }
    
    public ExternalNewObjectNode(ExternalCFGNode successor, ExternalCFGVariable def, JBCClass c) {
        super(successor, def);
        this.c = c;
    }
    
    public JBCClass getObjectClass() {
        return c;
    }
}
