/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.util.CompactSet;

public class JBCField implements DataConstants {
    private JBCClass inClass;
    private String name;
    private FieldData data;
    
    public JBCField(JBCClass inClass, String name) {
        this(inClass, name, null);
    }
    
    JBCField(JBCClass inClass, FieldData data) {
        this(inClass, data.getFieldName(), data);
    }
    
    private JBCField(JBCClass inClass, String name, FieldData data) {
        this.inClass = inClass;
        this.name = name;
        this.data = data;
    }
    
    public int hashCode() {
        return inClass.hashCode()*1734717 + name.hashCode()*193481;
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCField) {
            JBCField f = (JBCField)o;
            
            return f.name.equals(name) && f.inClass.equals(inClass);
        } else {
            return false;
        }
    }
    
    JBCField lookupFieldInClass(JBCClass c, CompactSet table) {
        JBCClass curClass = inClass;
        
        inClass = c;
        
        JBCField result = (JBCField)table.get(this);
        
        inClass = curClass;
        return result;
    }
    
    public FieldData getData() {
        if (data != null) {
            return data;
        } else {
            JBCField realField = inClass.getField(this);
            
            if (realField == null) {
                throw new NonexistentFieldError(this);
            } else {
                return realField.data;
            }
        }
    }
    
    public boolean isAccessibleTo(JBCClass c) {
        if (c.getWorld().isVerificationLenient()) {
            return true;
        } else {
            int flags = getData().getAccessFlags();
            
            if ((flags & ACC_PUBLIC) != 0) {
                return true;
            } else if ((flags & ACC_PRIVATE) != 0) {
                return inClass.equals(c);
            } else if ((flags & ACC_PROTECTED) != 0) {
                return c.isSamePackage(inClass) || c.isSubclassOf(inClass);
            } else {
                return c.isSamePackage(inClass);
            }
        }
    }
    
    public boolean exists() {
        if (data != null) {
            return true;
        } else {
            return inClass.getField(this) != null;
        }
    }
    
    void setObsolete() {
        data = null;
    }
    
    public boolean isStatic() {
        return (getData().getAccessFlags() & ACC_STATIC) != 0;
    }
    
    public boolean isTransient() {
        return (getData().getAccessFlags() & ACC_TRANSIENT) != 0;
    }
    
    public String getFieldName() {
        return name;
    }

    public String getFieldTypeName() {
        return getData().getFieldType();
    }
    
    public JBCType getFieldType() {
        return JBCType.parseType(getFieldTypeName(), inClass.getClassLoader());
    }
    
    public JBCClass getContainingClass() {
        return inClass;
    }
    
    public String toString() {
        return "field \"" + name + "\" of " + inClass;
    }
}
