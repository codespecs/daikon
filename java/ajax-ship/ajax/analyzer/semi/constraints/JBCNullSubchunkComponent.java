/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;

class JBCNullSubchunkComponent extends ComponentLabel {
    private static final JBCNullSubchunkComponent object
        = new JBCNullSubchunkComponent();
    
    private JBCNullSubchunkComponent() {
    }
    
    static JBCNullSubchunkComponent get() {
        return object;
    }
}
