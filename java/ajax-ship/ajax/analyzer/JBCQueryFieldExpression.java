/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

public class JBCQueryFieldExpression extends JBCCompoundExpression {
    private Object field;
    
    JBCQueryFieldExpression(JBCExpression base, Object field) {
        super(base);
        
        this.field = field;
    }
    
    public JBCExpression removeQueryField() {
        return getBase();
    }

    public Object getQueryField() {
        return field;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCQueryFieldExpression) {
            JBCQueryFieldExpression e = (JBCQueryFieldExpression)o;
            
            return e.field.equals(field) && e.getBase().equals(getBase());
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return field.hashCode()*37819 + getBase().hashCode()*34717 + 3418101;
    }
}
