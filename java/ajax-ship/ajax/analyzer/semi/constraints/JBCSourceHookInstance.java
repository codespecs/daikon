/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.InstanceLabel;

class JBCSourceHookInstance extends InstanceLabel {
    private static JBCSourceHookInstance instance = new JBCSourceHookInstance();
    
    private JBCSourceHookInstance() {
    }
    
    static JBCSourceHookInstance get() {
        return instance;
    }
    
    public String toString() {
        return "source hook";
    }
}
