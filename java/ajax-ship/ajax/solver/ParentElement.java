/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

public interface ParentElement {
    public Variable getParent();
    public ComponentLabel getLabel();
    
    public Variable getParentHead();
}
