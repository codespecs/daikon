/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.JBCField;

public class JBCFieldExpression extends JBCCompoundExpression {
    private JBCField field;
    
    public JBCField getField() {
        return field;
    }
    
    JBCFieldExpression(JBCExpression base, JBCField field) {
        super(base);
        
        this.field = field;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCFieldExpression) {
            JBCFieldExpression e = (JBCFieldExpression)o;
            
            return e.field.equals(field) && e.getBase().equals(getBase());
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return field.hashCode()*37819 + getBase().hashCode()*34717 + 38101;
    }
}
