/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

public interface InstanceElement {
    public Variable getInstance();
    public InstanceLabel getLabel();
    public int getPolarity();

    /* debugging only */
    public void validate(World w) throws ValidationException;
    
    public Variable getInstanceHead();
}
