/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semantics;

/*
This class represents the standard semantics of the value-point relation.
There are no combining operations; each created value has a unique identity
retained throughout its lifetime.
*/
public class StandardSemantics extends Semantics {
    private static StandardSemantics s = new StandardSemantics();
    
    private StandardSemantics() {
    }
    
    public static StandardSemantics get() {
        return s;
    }
}
