/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.jbc.JBCClass;

/**
This is an abstract value for objects when we know only the class of the object.
*/
class ClassOnlyObjectStackValue extends AbstractStackValue {
    private JBCClass objectClass;
    
    ClassOnlyObjectStackValue(JBCClass objectClass) {
        this.objectClass = objectClass;
    }
    
    boolean isValueKnown() {
        return false;
    }
    
    JBCClass getObjectClass(ConstraintManager manager) {
        return objectClass;
    }

    Object getObjectValue() {
        return null;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        return o == this;
    }
}
