/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

public class JBCLocalVarExpression extends JBCExpression {
    private int localVar;
    
    public int getLocalVarIndex() {
        return localVar;
    }
    
    JBCLocalVarExpression(int localVar) {
        this.localVar = localVar;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCLocalVarExpression) {
            JBCLocalVarExpression e = (JBCLocalVarExpression)o;
            
            return e.localVar == localVar;
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return localVar*37819 + 3148101;
    }
}
