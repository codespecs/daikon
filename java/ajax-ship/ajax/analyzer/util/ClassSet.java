/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.util.CompactSet;

class ClassSet {
    private JBCClass[] classes;
    
    ClassSet(JBCClass[] classes) {
        this.classes = classes;
    }
    
    JBCClass[] getClasses() {
        return classes;
    }
    
    public boolean equals(Object o) {
        if (o instanceof ClassSet) {
            ClassSet s = (ClassSet)o;
            JBCClass[] cs = s.getClasses();
            
            if (cs.length == classes.length) {
                if (cs.length <= 5) {
                    for (int i = 0; i < classes.length; i++) {
                        boolean found = false;
                        JBCClass c = classes[i];
                        
                        for (int j = 0; !found && j < cs.length; j++) {
                            if (c.equals(cs[j])) {
                                found = true;
                            }
                        }
                        
                        if (!found) {
                            return false;
                        }
                    }
                    
                    return true;
                } else {
                    CompactSet set = new CompactSet();
                    
                    for (int i = 0; i < classes.length; i++) {
                        set.addUnconditionally(classes[i]);
                    }
                    
                    for (int i = 0; i < cs.length; i++) {
                        if (set.get(cs[i]) == null) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        int value = 14890101;
        
        for (int i = 0; i < classes.length; i++) {
            value ^= classes[i].hashCode();
        }
        
        return value;
    }
}
