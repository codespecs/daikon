/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;

class SalamisParameterDefNode extends SalamisDefNode implements ExternalCFGParameterDefNode {
    private int index;
    
    SalamisParameterDefNode(SalamisParser p, String defVar, int index) {
        super(p, defVar);
        this.index = index;
    }
    
    public int getParameterIndex() {
        return index;
    }
}
