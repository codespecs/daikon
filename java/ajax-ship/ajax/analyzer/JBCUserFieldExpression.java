/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.UserField;

public class JBCUserFieldExpression extends JBCCompoundExpression {
    private UserField field;
    
    public UserField getField() {
        return field;
    }
    
    JBCUserFieldExpression(JBCExpression base, UserField field) {
        super(base);
        
        this.field = field;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCUserFieldExpression) {
            JBCUserFieldExpression e = (JBCUserFieldExpression)o;
            
            return e.field.equals(field) && e.getBase().equals(getBase());
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return field.hashCode()*37819 + getBase().hashCode()*34717 + 3811101;
    }
}
