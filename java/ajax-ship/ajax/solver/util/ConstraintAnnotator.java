/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.util;

import ajax.solver.*;

public interface ConstraintAnnotator {
    public String getVarLabel(Variable v);
    
    public void countSource();
    public void countInstance();
    public void countParent();
    public void countComponent();
}
