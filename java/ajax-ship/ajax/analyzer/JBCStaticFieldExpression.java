/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.jbc.JBCField;

public class JBCStaticFieldExpression extends JBCExpression {
    private JBCField field;
    
    JBCStaticFieldExpression(JBCField field) {
        this.field = field;
    }
    
    public JBCField getField() {
        return field;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCStaticFieldExpression) {
            JBCStaticFieldExpression e = (JBCStaticFieldExpression)o;
            
            return e.field.equals(field);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return field.hashCode()*37819 + 317898101;
    }
}
