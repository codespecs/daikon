/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer;

import ajax.Globals;
import ajax.jbc.*;
import ajax.util.IdentityManager;

public abstract class JBCExpression {
    JBCExpression() {
    }
    
    public JBCExpression removeQueryField() {
        return this;
    }
    
    public Object getQueryField() {
        return null;
    }
    
    public static JBCExpression makeStackElemExpression(int stackIndex) {
        return new JBCStackElemExpression(stackIndex);
    }
    
    public static JBCExpression makeLocalVarExpression(int localIndex) {
        return new JBCLocalVarExpression(localIndex);
    }
    
    public static JBCExpression makeFlowgraphDefNodeExpression() {
        return FlowgraphDefNodeExpression.get();
    }
    
    public static JBCExpression makeFlowgraphVarExpression(ExternalCFGVariable v) {
        return new FlowgraphVarExpression(v);
    }
    
    public JBCExpression makeUserFieldExpression(UserField field) {
        if (Globals.debug && field.isStatic()) {
            Globals.nonlocalError(field + " is unexpectedly static");
        }
        
        return new JBCUserFieldExpression(this, field);
    }
    
    public JBCExpression makeQueryFieldExpression(Object field) {
        return new JBCQueryFieldExpression(this, field);
    }
    
    public static JBCExpression makeStaticUserFieldExpression(UserField field) {
        if (Globals.debug && !field.isStatic()) {
            Globals.nonlocalError(field + " is unexpectedly nonstatic");
        }
        
        return new JBCStaticUserFieldExpression(field);
    }
    
    public JBCExpression makeNonstaticFieldExpression(JBCField field) {
        if (Globals.debug && field.isStatic()) {
            Globals.nonlocalError(field + " is unexpectedly static");
        }
        
        return new JBCFieldExpression(this, field);
    }
    
    public static JBCExpression makeStaticFieldExpression(JBCField field) {
        if (Globals.debug && !field.isStatic()) {
            Globals.nonlocalError(field + " is unexpectedly nonstatic");
        }
        
        return new JBCStaticFieldExpression(field);
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
