/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.Variable;

interface SpecialPredecessorRouting {
    public void rerouteVars(int successorNumber, int stackIndex, AbstractStackValue[] stackValues,
        Variable[] stackVars, AbstractStackValue[] localValues, Variable[] localVars);
}
