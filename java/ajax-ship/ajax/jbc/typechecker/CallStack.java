/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

import ajax.jbc.*;
import ajax.Globals;
import java.util.*;

class CallStack {
    private CallStack parent;
    private int returnTo;
    
    CallStack(CallStack parent, int returnTo) {
        this.parent = parent;
        this.returnTo = returnTo;
    }
    
    CallStack() {
        this(null, -1);
    }
    
    int getReturnAddress() {
        if (parent != null) {
            return returnTo;
        } else {
            throw Globals.localError("Root context returns nowhere!");
        }
    }
    
    CallStack getParent() {
        return parent;
    }
    
    boolean isRoot() {
        return parent == null;
    }
    
    public CallStack getRoot() {
        if (parent == null) {
            return this;
        } else {
            return parent.getRoot();
        }
    }
    
    public int hashCode() {
        return parent == null ? 190301010 : returnTo*1839119 + parent.hashCode()*138901;
    }
    
    public boolean equals(Object o) {
        if (o instanceof CallStack) {
            CallStack s = (CallStack)o;
            
            return parent == null ? s.parent == null
                : s.parent != null && s.parent.equals(parent) && s.returnTo == returnTo;
        } else {
            return false;
        }
    }
}
