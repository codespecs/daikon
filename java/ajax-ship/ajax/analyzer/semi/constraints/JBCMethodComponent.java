/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.jbc.JBCMethod;
import ajax.util.Association;
import ajax.Globals;

class JBCMethodComponent extends ComponentLabel {
    private JBCMethod m;
    
    private static Association map = new Association();
    
    private JBCMethodComponent(JBCMethod m) {
        if (Globals.debug) this.m = m;
    }
    
    public int getVariance() {
        return COVARIANT;
    }

    static JBCMethodComponent get(JBCMethod m) {
        Object obj = map.get(m);
        
        if (obj == null) {
            JBCMethodComponent result = new JBCMethodComponent(m);
            
            map.put(m, result);
            
            return result;
        } else {
            return (JBCMethodComponent)obj;
        }
    }
}
