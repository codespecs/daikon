/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.jbc.*;
import ajax.analyzer.*;
import ajax.jbc.util.*;

public class JBCValuePoint {
    private Location location;
    private JBCExpression expr;

    public JBCValuePoint(Location location, JBCExpression expr) {
        this.location = location;
        this.expr = expr;
    }
    
    public Location getLocation() {
        return location;
    }
    
    public JBCExpression getExpression() {
        return expr;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCValuePoint) {
            JBCValuePoint p = (JBCValuePoint)o;
            
            return p.location.equals(location) && p.expr.equals(expr);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return location.hashCode()*37191 + expr.hashCode()*41017 + 148912;
    }
}
