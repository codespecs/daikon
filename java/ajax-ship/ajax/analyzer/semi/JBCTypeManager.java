/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import ajax.jbc.*;
import java.util.*;
import ajax.solver.*;
import ajax.Globals;
import ajax.util.IdentityHashtable;

class JBCTypeManager {
    private IdentityHashtable varToType = new IdentityHashtable();
    private SEMIAnalyzer manager;
    
    JBCTypeManager(SEMIAnalyzer manager) {
        this.manager = manager;
    }
    
    void notifyVarsMerged(Variable to, Variable from) {
        Object fromTypeObj = varToType.remove(from);
        
        if (fromTypeObj != null) {
            setJBCType(to, (JBCType)fromTypeObj);
        }
    }
    
    void notifyNewInstance(Variable instance, Variable of) {
        Object ofTypeObj = varToType.get(of);
        
        if (ofTypeObj != null) {
            setJBCType(instance, (JBCType)ofTypeObj);
        } else {
            Object instanceTypeObj = varToType.get(instance);
        
            if (instanceTypeObj != null) {
                setJBCType(of, (JBCType)instanceTypeObj);
            }
        }
    }
    
    JBCType getJBCType(Variable v) {
        return (JBCType)varToType.get(v.getHead());
    }
    
    void setJBCType(Variable v, JBCType t) {
        Variable vHead = v.getHead();
        Object vTypeObj = varToType.get(vHead);
        
        if (Globals.debug && t.equals(JBCType.VOID)) {
            Globals.nonlocalError("Setting a variable to the VOID type");
        }
        
        if (vTypeObj == null) {
            World w = manager.getSolver();
            
            varToType.put(vHead, t);
            
            for (Enumeration e = vHead.getSources(w); e.hasMoreElements();) {
                setJBCType(((SourceElement)e.nextElement()).getSource(), t);
            }

            for (Enumeration e = vHead.getInstances(w); e.hasMoreElements();) {
                setJBCType(((InstanceElement)e.nextElement()).getInstance(), t);
            }
        } else if (!t.isEqualType((JBCType)vTypeObj)) {
            String msg = "A type error was detected in the Java bytecode at variable "
                + Globals.getHexID(vHead) + " (mismatched types " + t + " and " + vTypeObj + ");"
                + " either the bytecode is corrupt or an internal error has occurred.";
                
            if (Globals.debug) {
                msg = msg + "\nInternal constraint variables dumped to " + manager.dumpVarInfo(v);
            }
                
            Globals.nonlocalError(msg);
        }
    }
}
