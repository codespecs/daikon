/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;

class PredecessorEdgeInfo {
    int predOffset;
    InstanceLabel edgeInstance;
    Variable[] localsDefinedInBlock;
    PredecessorEdgeInfo next;
}
