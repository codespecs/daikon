/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.util.IdentityManager;

public class ComponentLabel {
    public ComponentLabel() {
    }
    
    public int getComponentIndex() {
        return -1;
    }
    
    public static final int COVARIANT     = 0;
    public static final int CONTRAVARIANT = 1;
    public static final int INVARIANT     = 2;    
    
    public int getVariance() {
        return INVARIANT;
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
