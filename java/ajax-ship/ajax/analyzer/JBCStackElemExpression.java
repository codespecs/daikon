/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

public class JBCStackElemExpression extends JBCExpression {
    private int stackElem;
    
    public int getStackElemIndex() {
        return stackElem;
    }
    
    JBCStackElemExpression(int stackElem) {
        this.stackElem = stackElem;
    }

    public boolean equals(Object o) {
        if (o instanceof JBCStackElemExpression) {
            JBCStackElemExpression e = (JBCStackElemExpression)o;
            
            return e.stackElem == stackElem;
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return stackElem*37819 + 31481;
    }
}
