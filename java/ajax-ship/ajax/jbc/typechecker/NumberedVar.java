/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.typechecker;

public class NumberedVar {
    private static NumberedVar unknown = new NumberedVar();
    
    protected NumberedVar() {
    }
    
    public static NumberedVar getUnknown() {
        return unknown;
    }
}
