/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

public interface ComponentElement {
    public Variable getComponent();
    public ComponentLabel getLabel();
    public boolean hasMode(int mode);
    
    /* debugging only */
    public void validate(World w) throws ValidationException;
    
    /* optimization --- same as getComponent().getHead() */
    public Variable getComponentHead();
}
