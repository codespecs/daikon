/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;

public abstract class SearchItem {
    SearchItem() {
    }
    
    public abstract JBCType getType();
}
