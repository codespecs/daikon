/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;
import java.util.*;
import ajax.jbc.ExternalCFGNode;

class ExternalCFGNodeInfo {
    Variable funVar = null;
    Variable[] defVars = null;
    Vector predecessors = new Vector(1);
    InstanceLabel[] predecessorEdgeInstances = null;
    Hashtable predecessorEdgesVarsCarried = null;
    Hashtable predecessorEdgesComponentsCarried = null;
    ExternalCFGNode blockHead;
}
