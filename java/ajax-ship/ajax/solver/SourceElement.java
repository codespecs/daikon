/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

public interface SourceElement {
    public Variable getSource();
    public InstanceLabel getLabel();
    public int getPolarity();

    public Variable getSourceHead();
}
