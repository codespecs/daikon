/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.JBCClass;

/**
This is an abstract value for objects when we know the precise value of the
object.
*/
class ObjectStackValue extends AbstractStackValue {
    private Object value;
    
    ObjectStackValue(Object value) {
        this.value = value;
    }
    
    boolean isValueKnown() {
        return true;
    }
    
    JBCClass getObjectClass(ConstraintManager manager) {
        return manager.getSpecialClass(value.getClass().getName());
    }

    Object getObjectValue() {
        return value;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        if (o instanceof ObjectStackValue) {
            ObjectStackValue v = (ObjectStackValue)o;
            
            return v.value == value;
        } else {
            return false;
        }
    }
}
