/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.solver.test;

import ajax.solver.InstanceLabel;

class PolyInstance extends InstanceLabel {
    private String name;
    
    PolyInstance(String name) {
        super();
        this.name = name;
    }
    
    String getName() {
        return name;
    }
    
    public String toString() {
        return name;
    }
}
