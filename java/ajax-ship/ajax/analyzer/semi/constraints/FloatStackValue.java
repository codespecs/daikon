/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

class FloatStackValue extends AbstractStackValue {
    private float value;
    
    FloatStackValue(float value) {
        this.value = value;
    }
    
    boolean isValueKnown() {
        return true;
    }
    
    float getFloatValue() {
        return value;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        if (o instanceof FloatStackValue) {
            FloatStackValue v = (FloatStackValue)o;
            
            return v.value == value;
        } else {
            return false;
        }
    }
}
