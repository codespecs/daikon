/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.semi;

import ajax.util.*;

class SEMIQueryFieldData {
    IdentityHashtable varToIntermediateUp = new IdentityHashtable(5);
    IdentityHashtable varToIntermediateDown = new IdentityHashtable(5);
    IdentityHashtable varToTargetCookies = new IdentityHashtable(5);
}
