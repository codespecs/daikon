/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

class ContextOffsetPair {
    private CallStack context;
    private int offset;
    
    ContextOffsetPair(CallStack context, int offset) {
        this.context = context;
        this.offset = offset;
    }
    
    int getOffset() {
        return offset;
    }
    
    CallStack getContext() {
        return context;
    }
    
    public int hashCode() {
        return offset*19031 + context.hashCode()*4719;
    }
    
    public boolean equals(Object o) {
        if (o instanceof ContextOffsetPair) {
            ContextOffsetPair p = (ContextOffsetPair)o;
            
            return p.offset == offset && p.context.equals(context);
        } else {
            return false;
        }
    }
}