/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.JBCClass;

/**
This is an abstract value for objects when we know nothing about the object.
*/
class UnknownObjectStackValue extends AbstractStackValue {
    UnknownObjectStackValue() {
    }
    
    boolean isValueKnown() {
        return false;
    }
    
    JBCClass getObjectClass(ConstraintManager manager) {
        return null;
    }

    Object getObjectValue() {
        return null;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        return o == this;
    }
}
