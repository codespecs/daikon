/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;

public class JBCMethodResultComponent extends ComponentLabel {
    private static JBCMethodResultComponent label =
        new JBCMethodResultComponent();
   
    private JBCMethodResultComponent() {
    }
   
    public static JBCMethodResultComponent get() {
        return label;
    }
    
    public int getVariance() {
        return CONTRAVARIANT;
    }
    
    public String toString() {
        return "method result";
    }
}
