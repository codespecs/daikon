/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.Variable;
import ajax.jbc.JBCClass;

class InstanceOfRouting implements SpecialPredecessorRouting {
    private int pathNum;
    private InstanceOfResultValue value;
    
    InstanceOfRouting(InstanceOfResultValue value, int pathNum) {
        this.value = value;
        this.pathNum = pathNum;
    }
    
    public void rerouteVars(int successorNumber, int stackSize, AbstractStackValue[] stackValues,
        Variable[] stackVars, AbstractStackValue[] localValues, Variable[] localVars) {
        AbstractStackValue sourceObject = value.getSourceValue();
        ConstraintManager manager = value.getManager();
        JBCClass c = value.getInstanceClass();
        
        for (int i = 0; i < stackSize; i++) {
            if (stackValues[i].isEqualValue(sourceObject)) {
                stackVars[i] = manager.makeDowncast(stackVars[i], c);
            }
        }

        for (int i = 0; i < localValues.length; i++) {
            if (localValues[i].isEqualValue(sourceObject)) {
                localVars[i] = manager.makeDowncast(localVars[i], c);
            }
        }
    }
}
