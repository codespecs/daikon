/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver;

import ajax.util.IdentityManager;

public class InstanceLabel {
    public InstanceLabel() {
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
