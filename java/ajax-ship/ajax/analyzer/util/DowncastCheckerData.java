/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.util.*;
import java.util.*;

public class DowncastCheckerData {
    private JBCClass bound;
    private Object curIntermediate = null;
    private boolean inError = false;
    
    public DowncastCheckerData(JBCLocation loc) {
        JBCMethod method = loc.getMethod();
        
        bound = JBCCodeUtilities.resolveInstructionClass(method,
            method.getData().getCode(), loc.getOffset());
    }
    
    public boolean isInError() {
        return inError;
    }
    
    public boolean isInBound() {
        return isInBound(curIntermediate, bound);
    }
    
    public static boolean isInBound(Object intermediate, JBCClass bound) {
        if (intermediate == null) {
            return true;
        } else if (bound == null) {
            return false;
        } else if (intermediate instanceof OverflowSet) {
            return bound.equals(bound.getClassLoader().getWorld().getSpecialClass("java.lang.Object"));
        } else if (intermediate instanceof CompactSet) {
            for (Enumeration e = ((CompactSet)intermediate).elements(); e.hasMoreElements();) {
                if (!ClassTracker.isContainedIn((JBCClass)e.nextElement(), bound)) {
                    return false;
                }
            }
            return true;
        } else {
            return ClassTracker.isContainedIn(intermediate, bound);
        }
    }
    
    public void updateResult(Object intermediate) {
        curIntermediate = intermediate;
        
        if (!inError && !isInBound(intermediate, bound)) {
            inError = true;
        }
    }
    
    public JBCClass getBound() {
        return bound;
    }
    
    public Object getIntermediate() {
        return curIntermediate;
    }
    
    public String getIntermediateString() {
        if (curIntermediate instanceof OverflowSet || curIntermediate instanceof CompactSet) {
            return BoundedSetTracker.toString(curIntermediate);
        } else {
            return ClassTracker.toString(curIntermediate);
        }
    }

    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
