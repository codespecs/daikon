/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;

public abstract class JBCCompoundExpression extends JBCExpression {
    private JBCExpression base;
    
    JBCCompoundExpression(JBCExpression base) {
        if (Globals.debug && base instanceof JBCQueryFieldExpression) {
            Globals.nonlocalError("A JBCQueryFieldExpression must have only one query field, at the top level");
        }
        
        this.base = base;
    }
    
    public JBCExpression getBase() {
        return base;
    }
}
