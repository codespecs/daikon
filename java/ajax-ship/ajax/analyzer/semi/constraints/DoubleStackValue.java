/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

class DoubleStackValue extends AbstractStackValue {
    private double value;
    
    DoubleStackValue(double value) {
        this.value = value;
    }
    
    boolean isValueKnown() {
        return true;
    }
    
    double getDoubleValue() {
        return value;
    }

    boolean isEqualValue(AbstractStackValue o) {
        if (o instanceof DoubleStackValue) {
            DoubleStackValue v = (DoubleStackValue)o;
            
            return v.value == value;
        } else {
            return false;
        }
    }
}
