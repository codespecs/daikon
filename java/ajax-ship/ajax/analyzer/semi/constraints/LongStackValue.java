/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi.constraints;

class LongStackValue extends AbstractStackValue {
    private long value;
    
    LongStackValue(long value) {
        this.value = value;
    }
    
    boolean isValueKnown() {
        return true;
    }
    
    long getLongValue() {
        return value;
    }
    
    boolean isEqualValue(AbstractStackValue o) {
        if (o instanceof LongStackValue) {
            LongStackValue v = (LongStackValue)o;
            
            return v.value == value;
        } else {
            return false;
        }
    }
}
