/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.Vector;
import ajax.util.WeakSet;
import ajax.jbc.*;
import java.io.*;

public class JBCMethodType extends JBCType {
    private static WeakSet set = new WeakSet();
    
    private boolean isStatic;
    private String name;
    private String type;
    private JBCType[] paramTypes = null;
    private JBCType returnType = null;
    private JBCClassLoader loader;
    private JBCClass owner;
    
    private JBCMethodType(JBCClassLoader loader, String name, String type, boolean isStatic, JBCClass owner) {
        this.name = name;
        this.type = type;
        this.isStatic = isStatic;
        this.loader = loader;
        this.owner = owner;
    }
    
    private void parseType() {
        StringReader r = new StringReader(type);
        Vector types = new Vector();
        boolean successful = false;

        try {
            if (r.read() == (int)'(') {
                JBCType paramType;
                
                while ((paramType = JBCType.parseType(r, loader)) != null) {
                    types.addElement(paramType);
                }
                
                if (r.read() == (int)')') {
                    JBCType rType = JBCType.parseType(r, loader);
                    
                    if (rType != null && r.read() == -1) {
                        successful = true;
                        if (isStatic) {
                            paramTypes = new JBCType[types.size()];
                            types.copyInto(paramTypes);
                        } else {
                            paramTypes = new JBCType[types.size() + 1];
                            paramTypes[0] = owner == null ? JBCType.OBJECT
                                : owner.getClassType();
                            for (int i = 1; i < paramTypes.length; i++) {
                                paramTypes[i] = (JBCType)types.elementAt(i - 1);
                            }
                        }
                        returnType = rType;
                    }
                }
            }
        } catch (IOException ex) {
        }
        
        if (!successful) {
            throw new InvalidClassDataError("Invalid type: " + type);
        }
    }

/**
Get the list of parameter types for this method. If the method is nonstatic
then the "this" type is included as the first parameter.

@return an array of JBCTypes
*/
    public JBCType[] getParameterTypes() {
        if (paramTypes == null) {
            parseType();
        }
        
        return paramTypes;
    }
    
    public JBCType getReturnType() {
        if (returnType == null) {
            parseType();
        }
        
        return returnType;
    }
    
    public boolean isEqualType(JBCType t) {
        if (t instanceof JBCMethodType) {
            JBCMethodType methodType = (JBCMethodType)t;
            
            return methodType.name.equals(name) && methodType.type.equals(type)
                && methodType.isStatic == isStatic;
        } else {
            return false;
        }
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCMethodType) {
            JBCMethodType t = (JBCMethodType)o;
            
            return t.name.equals(name) && t.type.equals(type)
                && t.isStatic == isStatic && t.loader.equals(loader)
                && (owner == null ? t.owner == null : owner.equals(t.owner));
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return name.hashCode()*179341 + type.hashCode()*-123
            + (isStatic ? 18948191 : 819819417)
            + (owner == null ? 0 : owner.hashCode()*1031)
            + loader.hashCode()*11127;
    }
    
    public static JBCMethodType get(JBCClassLoader loader, String name, String type, boolean isStatic) {
        return (JBCMethodType)set.add(new JBCMethodType(loader, name, type, isStatic, null));
    }
    
    public static JBCMethodType get(JBCMethod m) {
        return (JBCMethodType)set.add(new JBCMethodType(
                m.getContainingClass().getClassLoader(), m.getMethodName(),
                m.getMethodTypeName(), m.isStatic(), m.getContainingClass()));
    }
    
    public String toString() {
        JBCType[] paramTypes = getParameterTypes();
        StringBuffer buf = new StringBuffer();
        int firstParam;
        
        if (isStatic) {
            firstParam = 0;
            buf.append("static ");
        } else {
            firstParam = 1;
        }
        
        buf.append(getReturnType());
        buf.append(" ");
        buf.append(name);
        buf.append("(");
        
        for (int i = firstParam; i < paramTypes.length; i++) {
            if (i > firstParam) {
                buf.append(", ");
            }
            buf.append(paramTypes[i]);
        }
        
        buf.append(")");
        
        return buf.toString();
    }
}
