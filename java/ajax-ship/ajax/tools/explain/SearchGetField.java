/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;
import ajax.jbc.util.*;

public class SearchGetField extends SearchAccessField {
    SearchGetField(Location location) {
        super(location);
    }
    
    public String toString() {
        return "Read from " + getField() + " at " + getLocation();
    }
}
