/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.jbc.ExternalCFGVariable;
import ajax.Globals;

class ExternalCFGVariableComponent extends ComponentLabel {
    private ExternalCFGVariable variable;
    
    ExternalCFGVariableComponent(ExternalCFGVariable variable) {
        if (Globals.debug) this.variable = variable;
    }
}
