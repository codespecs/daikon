/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.UserField;
import ajax.solver.ComponentLabel;
import ajax.util.Association;
import ajax.Globals;

class JBCUserFieldComponent extends ComponentLabel {
    private UserField f;
    
    private static Association map = new Association();
    
    private JBCUserFieldComponent(UserField f) {
        if (Globals.debug) this.f = f;
    }
    
    static JBCUserFieldComponent get(UserField f) {
        Object obj = map.get(f);
        
        if (obj == null) {
            JBCUserFieldComponent result = new JBCUserFieldComponent(f);
            
            map.put(f, result);
            
            return result;
        } else {
            return (JBCUserFieldComponent)obj;
        }
    }
    
    public String toString() {
        if (f != null) {
            return f.toString();
        } else {
            return "No_name_available";
	}
    }
}
