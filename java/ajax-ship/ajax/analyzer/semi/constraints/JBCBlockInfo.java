/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import java.util.Hashtable;
import ajax.solver.Variable;

class JBCBlockInfo {
    int offset;
    PredecessorEdgeInfo firstPredecessor = null;
    JBCReturnInfo returnInfo;
    int predecessorsNotProcessed;
    
/* The following fields are filled in or modified by each predecessor */
    Variable exceptionVar = null;
    Variable returnVar;
    Variable globalsVar;
    Variable[] entryStackVars;
    Variable[] entryLocalVars;
    AbstractStackValue[] entryStackValues;
    AbstractStackValue[] entryLocalValues;
}
