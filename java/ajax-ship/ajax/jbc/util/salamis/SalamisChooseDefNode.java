/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util.salamis;

import ajax.jbc.*;

class SalamisChooseDefNode extends SalamisDefNode implements ExternalCFGChoiceDefNode {
    private ExternalCFGVariable[] vars;
    
    SalamisChooseDefNode(SalamisParser p, String defVar, ExternalCFGVariable[] vars) {
        super(p, defVar);
        this.vars = vars;
    }

    public ExternalCFGVariable[] getChoiceValues() {
        return vars;
    }
}
