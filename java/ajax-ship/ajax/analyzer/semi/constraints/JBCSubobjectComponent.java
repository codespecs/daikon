/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.jbc.JBCClass;
import ajax.util.Association;
import ajax.Globals;

class JBCSubobjectComponent extends ComponentLabel {
    private JBCClass c;
    
    private static Association map = new Association();
    
    private JBCSubobjectComponent(JBCClass c) {
        if (Globals.debug) this.c = c;
    }
    
    static JBCSubobjectComponent get(JBCClass c) {
        Object obj = map.get(c);
        
        if (obj == null) {
            JBCSubobjectComponent result = new JBCSubobjectComponent(c);
            
            map.put(c, result);
            
            return result;
        } else {
            return (JBCSubobjectComponent)obj;
        }
    }
    
    public int getVariance() {
        return COVARIANT;
    }

    public String toString() {
        if (Globals.debug) {
            return "subobject for " + c;
        } else {
            return super.toString();
        }
    }
}
