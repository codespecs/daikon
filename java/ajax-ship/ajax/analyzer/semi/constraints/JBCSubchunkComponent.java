/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.JBCClass;
import ajax.util.Association;
import ajax.solver.ComponentLabel;
import ajax.Globals;

class JBCSubchunkComponent extends ComponentLabel {
    private JBCClass c;
    
    private static Association map = new Association();
    
    private JBCSubchunkComponent(JBCClass c) {
        if (Globals.debug) this.c = c;
    }
    
    static JBCSubchunkComponent get(JBCClass c) {
        Object obj = map.get(c);
        
        if (obj == null) {
            JBCSubchunkComponent result = new JBCSubchunkComponent(c);
            
            map.put(c, result);
            
            return result;
        } else {
            return (JBCSubchunkComponent)obj;
        }
    }
    
    public int getVariance() {
        return COVARIANT;
    }
    
    public String toString() {
        if (Globals.debug) {
            return "subchunk for " + c;
        } else {
            return super.toString();
        }
    }
}
