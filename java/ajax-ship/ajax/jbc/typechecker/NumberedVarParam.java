/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

public class NumberedVarParam extends NumberedVar {
    private int index;
    
    NumberedVarParam(int index) {
        this.index = index;
    }
    
    public int getIndex() {
        return index;
    }
    
    public boolean equals(Object o) {
        if (o instanceof NumberedVarParam) {
            NumberedVarParam n = (NumberedVarParam)o;
            
            return n.index == index;
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return 418910117 + index;
    }
}
