/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.util.IdentityManager;

/**
This class is an abstraction of invocation contexts.

Currently, an invocation context consists of the call site. The
call site info is contained in a subclass. This can be configured below.
*/
public class InvocationContext {
    public static final boolean indexByFullContext = false; // CONFIG
    
    InvocationContext(InvocationContext parent) {
    }
    
    InvocationContext(ConstraintManager manager) {
    }
    
    public String toString() {
        return "unknown context";
    }
    
    public int hashCode() {
        if (indexByFullContext) {
            return IdentityManager.getIdentityHashCode(this);
        } else {
            return 190480156;
        }
    }
    
    public boolean equals(Object o) {
        if (indexByFullContext) {
            return o == this;
        } else {
            return o instanceof InvocationContext;
        }
    }
}
