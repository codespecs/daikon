/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;

public class JBCMethodExceptionComponent extends ComponentLabel {
    private static JBCMethodExceptionComponent label =
        new JBCMethodExceptionComponent();
   
    private JBCMethodExceptionComponent() {
    }
   
    public static JBCMethodExceptionComponent get() {
        return label;
    }
    
    public int getVariance() {
        return CONTRAVARIANT;
    }
    
    public String toString() {
        return "method exception";
    }
}
