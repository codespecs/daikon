/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

class IntStackValue extends AbstractStackValue {
    private int value;
    
    IntStackValue(int value) {
        this.value = value;
    }
    
    boolean isValueKnown() {
        return true;
    }
    
    int getIntValue() {
        return value;
    }

    boolean isEqualValue(AbstractStackValue o) {
        if (o instanceof IntStackValue) {
            IntStackValue v = (IntStackValue)o;
            
            return v.value == value;
        } else {
            return false;
        }
    }
}
