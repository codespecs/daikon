/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.solver.ComponentLabel;
import ajax.jbc.JBCField;
import ajax.util.Association;
import ajax.Globals;

class JBCFieldComponent extends ComponentLabel {
    private JBCField f;
    
    private static Association map = new Association();
    
    private JBCFieldComponent(JBCField f) {
        if (Globals.debug) this.f = f;
    }
    
    static JBCFieldComponent get(JBCField f) {
        Object obj = map.get(f);
        
        if (obj == null) {
            JBCFieldComponent result = new JBCFieldComponent(f);
            
            map.put(f, result);
            
            return result;
        } else {
            return (JBCFieldComponent)obj;
        }
    }
    
    public String toString() {
        if (Globals.debug) {
            return f.toString();
        } else {
            return "field";
        }
    }
}
