/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import ajax.jbc.util.*;

public class ExternalParameterNode extends ExternalDefNode implements ExternalCFGParameterDefNode, java.io.Serializable {
    private int index;
    
    public ExternalParameterNode(int index) {
        this.index = index;
    }
    
    public ExternalParameterNode(ExternalCFGNode successor, int index) {
        super(successor);
        this.index = index;
    }
    
    public ExternalParameterNode(ExternalCFGVariable def, int index) {
        super(def);
        this.index = index;
    }
    
    public ExternalParameterNode(ExternalCFGNode successor, ExternalCFGVariable def, int index) {
        super(successor, def);
        this.index = index;
    }
    
    public int getParameterIndex() {
        return index;
    }
}