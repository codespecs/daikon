/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;

public class SearchActualParam extends SearchItem {
    private Location location;
    private int paramIndex;
    
    SearchActualParam(Location location, int paramIndex) {
        this.location = location;
        this.paramIndex = paramIndex;
    }
    
    public JBCType getType() {
        return getCallee().getMethodType().getParameterTypes()[paramIndex];
    }
    
    public Location getLocation() {
        return location;
    }
    
    public int getParamIndex() {
        return paramIndex;
    }
    
    public JBCMethod getCallee() {
        return location.getCalledMethod();
    }
    
    public String toString() {
        return "Actual parameter " + getParamIndex() + " in call to " + getCallee()
            + " at " + getLocation();
    }

    public int hashCode() {
        return location.hashCode()*130143 + paramIndex*1491 + 104731;
    }
    
    public boolean equals(Object o) {
        if (o instanceof SearchActualParam) {
            SearchActualParam p = (SearchActualParam)o;
            
            return p.location.equals(location) && p.paramIndex == paramIndex;
        } else {
            return false;
        }
    }
}
