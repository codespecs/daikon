/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

public class NumberedVarComputation extends NumberedVar {
    private int offset;
    private int num;
    
    NumberedVarComputation(int offset, int num) {
        this.offset = offset;
        this.num = num;
    }
    
    public int getOffset() {
        return offset;
    }
    
    public int getComputationNumber() {
        return num;
    }
    
    public boolean equals(Object o) {
        if (o instanceof NumberedVarComputation) {
            NumberedVarComputation n = (NumberedVarComputation)o;
            
            return n.offset == offset && n.num == num;
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return 1389014801 + offset*348071 + num;
    }
}
