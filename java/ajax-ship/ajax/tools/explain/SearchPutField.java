/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;
import ajax.jbc.util.*;

public class SearchPutField extends SearchAccessField {
    SearchPutField(Location location) {
        super(location);
    }
    
    public String toString() {
        return "Write to " + getField() + " at " + getLocation();
    }
}
