/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.util.CompactSet;

/**
A JBCMethod is a reference to a method declared in a class.
The method may or may not actually exist.
*/
public class JBCMethod implements DataConstants {
    private JBCClass inClass;
    private String methodName;
    private String methodType;
    private int hashValue;
    
/**
This field can only be non-null in objects that are part of
inClass's standard method table.
*/
    private MethodData data;
    
    public JBCMethod(JBCClass inClass, String methodName, String methodType) {
        this(inClass, methodName, methodType, null);
    }
    
    JBCMethod(JBCClass inClass, MethodData data) {
        this(inClass, data.getMethodName(), data.getMethodType(), data);
    }
   
    private JBCMethod(JBCClass inClass, String methodName, String methodType,
        MethodData data) {
        this.inClass = inClass;
        this.methodName = methodName;
        this.methodType = methodType;
        this.data = data;
        
        makeHashValue();
    }
    
    public boolean equals(Object o) {
        if (o instanceof JBCMethod) {
            JBCMethod m = (JBCMethod)o;
            
            return m.methodName.equals(methodName) &&
                m.methodType.equals(methodType) && m.inClass.equals(inClass);
        } else {
            return false;
        }
    }
    
    public int hashCode() {
        return hashValue;
    }
    
    public JBCClass getContainingClass() {
        return inClass;
    }
    
    void setObsolete() {
        data = null;
    }
    
    private void makeHashValue() {
        hashValue = inClass.hashCode()*16783 +
            methodName.hashCode()*17341 + methodType.hashCode()*131475;
    }
    
    JBCMethod lookupMethodInClass(JBCClass c, CompactSet table) {
        int curHashValue = hashValue;
        JBCClass curClass = inClass;
        
        inClass = c;
        makeHashValue();
        
        JBCMethod result = (JBCMethod)table.get(this);
        
        hashValue = curHashValue;
        inClass = curClass;
        return result;
    }
    
    public MethodData getData() {
        if (data != null) {
            return data;
        } else {
            JBCMethod realMethod = inClass.getMethod(this);
            
 /* Note that we do not assign to 'data' here. That's because we need
    to be able to smash all 'data' fields if a class changes and some
    methods cease to exist. It's easiest to do that if we only allow
    JBCMethods in class method tables to have non-null 'data' fields. */
            if (realMethod == null) {
                throw new NonexistentMethodError(this);
            } else {
                return realMethod.data;
            }
        }
    }
    
    public boolean exists() {
        if (data != null) {
            return true;
        } else {
            return inClass.getMethod(this) != null;
        }
    }
    
    public String getMethodName() {
        return methodName;
    }
    
    public String getMethodTypeName() {
        return methodType;
    }
    
    public JBCMethodType getMethodType() {
        return JBCMethodType.get(this);
    }
    
    public ExternalFlowgraph getNativeCode() {
        int flags = getData().getAccessFlags();
        
        if ((flags & ACC_NATIVE) == 0) {
            return null;
        } else {
            return inClass.getClassLoader().getWorld().getNativeCode(this);
        }
    }
    
    public boolean isStatic() {
        return (getData().getAccessFlags() & ACC_STATIC) != 0;
    }
    
    public boolean isNative() {
        return (getData().getAccessFlags() & ACC_NATIVE) != 0;
    }
    
    public boolean isSynchronized() {
        return (getData().getAccessFlags() & ACC_SYNCHRONIZED) != 0;
    }
    
    public boolean isAbstract() {
        return (getData().getAccessFlags() & ACC_ABSTRACT) != 0;
    }
    
    public boolean isPrivate() {
        return (getData().getAccessFlags() & ACC_PRIVATE) != 0;
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
    
    public String toString() {
        try {
            return "method " + getMethodType().toString() + " in " + inClass;
        } catch (NonexistentMethodError ex) {
            return "Method " + getMethodName() + " (" + getMethodTypeName() + ") in class "
                + getContainingClass().getClassName();
        }
    }
}
