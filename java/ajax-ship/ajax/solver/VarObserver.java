/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

public interface VarObserver {
/**
This method is called just before two variables are merged.
They are both guaranteed to be the representative 'head' variables.
'to' will be the new head.

Warning: DO NOT attempt to manipulate the constraint system at this
time; it may be in a temporarily inconsistent state.
*/
    public void notifyVarsMerged(Variable to, Variable from);

/**
Warning: DO NOT attempt to manipulate the constraint system at this
time; it may be in a temporarily inconsistent state.
*/    
    public void notifyNewInstance(Variable instance, Variable of);

/**
Warning: DO NOT attempt to manipulate the constraint system at this
time; it may be in a temporarily inconsistent state.
*/    
    public void notifyNewComponent(Variable component, Variable of);
}
