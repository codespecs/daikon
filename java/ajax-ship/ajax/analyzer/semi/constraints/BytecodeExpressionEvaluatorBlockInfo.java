/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.*;
import java.util.Vector;

class BytecodeExpressionEvaluatorBlockInfo {
/** The predecessor indices are needed so that local variables can be routed. */
    int[] predecessors;
/** The predecessor instances are needed so that local variables can be routed. */
    InstanceLabel[] predecessorInstances;
/** The returns-to-this indices are needed so that local variables can be routed. */
    int[] returnersToThis;
    Variable returnAddressTuple;
/** The Variable for each local variable that has been routed from this block.
    Some elements may be null; these have not yet been routed. */
    Variable[] localVars;
/** The Variable for each stackElement that has been routed from this block.
    None of these elements are null; they have all been routed. */
    Variable[] stackVars;
    
    BytecodeExpressionEvaluatorBlockInfo(JBCInstructionInfo info) {
        throw new Error("unimplemented");
    }
}
