/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;

class JBCMethodGlobalsArgComponent extends ComponentLabel {
    private static JBCMethodGlobalsArgComponent label =
        new JBCMethodGlobalsArgComponent();
   
    private JBCMethodGlobalsArgComponent() {
    }
   
    static JBCMethodGlobalsArgComponent get() {
        return label;
    }
    
    public int getVariance() {
        return COVARIANT;
    }

    public String toString() {
        return "method globals arg";
    }
}
