/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.explain;

import ajax.Globals;
import ajax.jbc.*;
import ajax.jbc.util.*;

public class SearchAccessField extends SearchItem implements OpcodeConstants {
    private Location location;
    
    SearchAccessField(Location location) {
        this.location = location;
    }
    
    public JBCType getType() {
        return getField().getFieldType();
    }
    
    public JBCField getField() {
        return location.getAccessedField();
    }
    
    public Location getLocation() {
        return location;
    }
    
    public int hashCode() {
        return location.hashCode()*130143 + 1049;
    }
    
    public boolean equals(Object o) {
        if (o instanceof SearchAccessField) {
            SearchAccessField f = (SearchAccessField)o;
            
            return f.location.equals(location);
        } else {
            return false;
        }
    }
}
