/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

import ajax.Globals;
import ajax.jbc.JBCClass;

class AbstractStackValue {
    SpecialPredecessorRouting notifyConsumed(int consumerOffset) {
        return null;
    }
    
    boolean isValueKnown() {
        return false;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        return false;
    }
    
    int getIntValue() {
        throw new InvalidAbstractValueTypeError("Value is not an int");
    }
    
    long getLongValue() {
        throw new InvalidAbstractValueTypeError("Value is not a long");
    }
    
    float getFloatValue() {
        throw new InvalidAbstractValueTypeError("Value is not a float");
    }
    
    double getDoubleValue() {
        throw new InvalidAbstractValueTypeError("Value is not a double");
    }
    
    JBCClass getObjectClass(ConstraintManager manager) {
        throw new InvalidAbstractValueTypeError("Value is not an Object");
    }

    Object getObjectValue() {
        throw new InvalidAbstractValueTypeError("Value is not an Object");
    }
}
