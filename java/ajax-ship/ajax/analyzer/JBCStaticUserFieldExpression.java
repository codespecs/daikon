/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.UserField;

public class JBCStaticUserFieldExpression extends JBCExpression {
    private UserField field;
    
    JBCStaticUserFieldExpression(UserField field) {
        this.field = field;
    }
    
    public UserField getField() {
        return field;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCStaticUserFieldExpression) {
            JBCStaticUserFieldExpression e = (JBCStaticUserFieldExpression)o;
            
            return e.field.equals(field);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return field.hashCode()*37819 + 31784101;
    }
}
