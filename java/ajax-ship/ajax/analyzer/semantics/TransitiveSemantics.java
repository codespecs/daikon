/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semantics;

import ajax.jbc.util.*;

/*
Transitive closure semantics.

This semantics has the property that, for all e1,e2,
if loc:e1 <--> loc:e2 then
    for all e. e <--> loc:e1 iff e <--> loc:e2
    for all e, F. e <--> loc:e1.F iff e <--> loc:e2.F
*/
public class TransitiveSemantics extends Semantics {
    private Location loc;
    
    public TransitiveSemantics(Location loc) {
        this.loc = loc;
    }
    
    public Location getLocation() {
        return loc;
    }
}
