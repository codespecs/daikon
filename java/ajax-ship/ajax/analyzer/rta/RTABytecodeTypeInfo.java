/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.rta;

import ajax.analyzer.*;
import ajax.jbc.*;
import ajax.jbc.typechecker.*;
import ajax.Globals;
import java.util.*;
import ajax.util.*;

class RTABytecodeTypeInfo implements EnumerationMapper {
    private BytecodeTypechecker checker;
    private boolean distinctPrimitives;
    
    RTABytecodeTypeInfo(BytecodeTypechecker checker, boolean distinctPrimitives) {
        this.checker = checker;
        this.distinctPrimitives = distinctPrimitives;
    }
    
    public Object map(Object o) {
        if (o instanceof JBCType) {
            JBCType t = (JBCType)o;
            
            if (t instanceof JBCObjectType) {
                if (t == JBCType.OBJECT) {
                    // This is the "null" type.
                    // We don't handle nulls; they're just ignored.
                    // The value-point relation is defined to exclude null values.
                    return null;
                } else {
                    return ((JBCObjectType)t).getClassDef();
                }
            } else if (t.isEqualType(JBCType.INT)) {
                /* this did a "weak check", i.e. it's true for boolean, byte, char, short also */
                return JBCType.INT;
            } else if (t instanceof ReturnAddrType) {
                return JBCType.RETURNADDR;
            } else if (distinctPrimitives) {
                return t;
            } else {
                return JBCType.INT;
	    }
        } else {
            return o;
        }
    }
    
    private Enumeration convertTypes(Enumeration types) {
        return new EnumerationMap(this, types);
    }
    
    Enumeration getTypesAt(int offset, JBCExpression expression) {
        if (expression instanceof JBCLocalVarExpression) {
            return convertTypes(checker.getLocalVarTypes(offset,
                    ((JBCLocalVarExpression)expression).getLocalVarIndex())); 
        } else if (expression instanceof JBCStackElemExpression) {
            return convertTypes(checker.getStackElemTypes(offset,
                    ((JBCStackElemExpression)expression).getStackElemIndex())); 
        } else if (expression instanceof JBCStaticFieldExpression) {
            return convertTypes(new SingletonEnumerator(
                    ((JBCStaticFieldExpression)expression).getField().getFieldType()));
        } else if (expression instanceof JBCFieldExpression) {
            return convertTypes(new SingletonEnumerator(
                    ((JBCFieldExpression)expression).getField().getFieldType()));
        } else {
            if (Globals.debug && !(expression instanceof JBCStaticUserFieldExpression)
                && !(expression instanceof JBCUserFieldExpression)) {
                Globals.nonlocalError("Unknown expression type!");
            }
            
            return new SingletonEnumerator(RTA.TOP);
        }
    }
}
