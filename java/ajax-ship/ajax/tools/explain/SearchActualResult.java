/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.jbc.*;
import ajax.jbc.util.*;

public class SearchActualResult extends SearchItem {
    private Location location;
    
    SearchActualResult(Location location) {
        this.location = location;
    }
    
    public JBCType getType() {
        return getCallee().getMethodType().getReturnType();
    }
    
    public Location getLocation() {
        return location;
    }
    
    public JBCMethod getCallee() {
        return location.getCalledMethod();
    }
    
    public String toString() {
        return "Actual result from call to " + getCallee()
            + " from " + location;
    }

    public int hashCode() {
        return location.hashCode() + 10161;
    }
    
    public boolean equals(Object o) {
        if (o instanceof SearchActualResult) {
            SearchActualResult p = (SearchActualResult)o;
            
            return p.location.equals(location);
        } else {
            return false;
        }
    }
}
