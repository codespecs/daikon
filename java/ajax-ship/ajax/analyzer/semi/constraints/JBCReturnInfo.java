/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;

class JBCReturnInfo {
    int[] retToThisInstructionOffsets;
    Variable jsrTupleVar;
    InstanceLabel instance;
}
