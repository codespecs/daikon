/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import java.util.Hashtable;
import ajax.solver.ComponentLabel;

class ExternalCFGVariableInfo {
    Hashtable defNodeHeads = new Hashtable();
    ComponentLabel component = null;
}
