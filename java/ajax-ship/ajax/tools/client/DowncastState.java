/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.client;

import ajax.tools.protocol.*;
import javax.swing.tree.*;
import java.awt.Color;

class DowncastState {
    private int state = UNKNOWN;
    private ClassDescriptor bound;
    private ClassDescriptor actual; // if SAFE
    
    static final int UNKNOWN = 0;
    static final int SAFE = 1;
    static final int UNSAFE = 2;
    
    DowncastState(ClassDescriptor bound) {
        this.bound = bound;
    }
    
    int getState() {
        return state;
    }
    
    ClassDescriptor getBound() {
        return bound;
    }
    
    Color getColor() {
        switch (state) {
            case SAFE: return Color.green;
            case UNSAFE: return Color.red;
            default: return null;
        }
    }
    
    void setSafe(ClassDescriptor actual) {
        state = SAFE;
        this.actual = actual;
    }
    
    void setUnknown() {
        state = UNKNOWN;
        this.actual = null;
    }
    
    void setUnsafe() {
        state = UNSAFE;
        this.actual = null;
    }
}
