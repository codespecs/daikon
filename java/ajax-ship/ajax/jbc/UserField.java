/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

public class UserField {
    private JBCClass containingClass;
    private boolean isStaticField;
    private String name;
    
    UserField(JBCClass containingClass, boolean isStaticField, String name) {
        this.containingClass = containingClass;
        this.isStaticField = isStaticField;
        this.name = name;
    }
    
    public String getFieldName() {
        return name;
    }
    
    public JBCClass getContainingClass() {
        return containingClass;
    }
    
    public boolean isStatic() {
        return isStaticField;
    }
    
    public String toString() {
        return "Userfield \"" + name + "\" in " + containingClass;
    }
    
    public boolean equals(Object o) {
        if (o instanceof UserField) {
            UserField f = (UserField)o;
            
            return f.containingClass.equals(containingClass)
                && f.isStaticField == isStaticField
                && f.name.equals(name);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return containingClass.hashCode()*3411 + name.hashCode()*30101 +
            + (isStaticField ? 1091 : 101010);
    }
}
