/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.util.*;
import ajax.Globals;
import ajax.util.*;

/**
An object of this class represents a class in a program under analysis.
Each JBCClass belongs to a JBCWorld.
*/
public class JBCClass implements DataConstants {
    private JBCClassLoader loader;
    private ClassData data;
    private CompactSet methods;
    private CompactSet fields;
    private Hashtable userFields = null;
    private JBCClass superClass;
    private JBCClass[] superInterfaces;
    private JBCMethod initializerMethod;
    private String className = null;
    
    JBCClass(JBCClassLoader loader, ClassData data) {
        this.loader = loader;
        this.data = data;
        
        init();

        loader.getWorld().notifyClassLoaded(this);
    }
    
    private void init() {
        setupMethods();
        setupFields();
        setupSuperClass();
        setupSuperInterfaces();
        
        int flags = getData().getAccessFlags();
        
        if ((flags & ACC_ABSTRACT) == 0) {
            if ((flags & ACC_INTERFACE) != 0) {
                throw new InvalidClassDataError("Interface "
                    + getClassName() + " must be abstract (JVM Spec 4.1)");
            }

            checkAllMethodsImplemented(this, new CompactSet());
        } else if ((flags & ACC_FINAL) != 0) {
            throw new InvalidClassDataError("Class "
                + getClassName() + " cannot be both abstract and final (JVM Spec 4.1)");
        }
        
        if (Globals.debug) {
            getClassName();
        }
    }        
    
/**
Retrieves the class loader that generated this class.

@return the class loader
*/
    public JBCClassLoader getClassLoader() {
        return loader;
    }
    
/**
Retrieves the raw data that was used to generate this class

@return the class data
*/
    public ClassData getData() {
        return data;
    }

    private void setupSuperClass() {
        String superClassName = getData().getSuperClassName();
    
        if (superClassName != null) {
            superClass = loader.getClass(superClassName);
            
            if (superClass == null) {
                throw new InvalidClassDataError("Superclass " +
                    superClassName + " of class "
                    + getClassName() + " not found");
            } else if (superClass.isInterface()) {
                throw new InvalidClassDataError("Superclass " +
                    superClassName + " of class "
                    + getClassName() + " is an interface");
            } else if (superClass.isFinal()) {
                throw new InvalidClassDataError("Superclass " +
                    superClassName + " of class "
                    + getClassName() + " is final");
            }                
        } else {
            superClass = null;
        }
    }
    
/**
Retrieves the class name, fully qualified with the package name, in
dot-separated format.

@return the name
*/
    public String getClassName() {
        if (className == null) {
            className = getData().getClassName();
        }

        return className;
    }
    
/**
Retrieves the type of objects of this class.

@return the type
*/
    public JBCObjectType getClassType() {
        return JBCObjectType.get(this);
    }
    
/**
Retrieves the superclass of this class.

@return the superclass
*/
    public JBCClass getSuperClass() {
        return superClass;
    }
    
/**
Retrieves the JBCWorld that this class belongs to.

@return the world
*/
    public JBCWorld getWorld() {
        return loader.getWorld();
    }

/**
Retrieves the name of the source file that this class was compiled from,
or null if the name is unknown.

@return the source file name
*/
    public String getSourceFileName() {
        return data.getSourceFileName();
    }

    private void setupSuperInterfaces() {
        String[] names = getData().getSuperInterfaceNames();
        JBCClass[] interfaces = new JBCClass[names.length];
        
        for (int i = 0; i < interfaces.length; i++) {
            JBCClass iface = loader.getClass(names[i]);
            
            if (iface == null) {
                throw new InvalidClassDataError("Superinterface " +
                    names[i] + " of class "
                    + getClassName() + " not found");
            } else if (!iface.isInterface()) {
                throw new InvalidClassDataError("Superinterface " +
                    iface + " of class "
                    + getClassName() + " is not an interface");
            }
            
            interfaces[i] = iface;
        }
        
        superInterfaces = interfaces;
    }
    
/**
Registers a user field of this class. This is a special hidden field
that can be used by native code specifications only to associate shadow data
with objects, or to store global data (if isStatic is true). This method
can be called multiple times with the same name, in which case it will
return the same UserField each time. However, just like regular fields,
a class cannot have a static and a nonstatic user field with the same name.

@param name the name of the field
@param isStatic true if the field is global, false if the field is an instance field
@return the user field
*/
    public UserField registerUserField(String name, boolean isStatic) {
        if (userFields == null) {
            userFields = new Hashtable();
        }
        
        UserField f = (UserField)userFields.get(name);
        
        if (f == null) {
            f = new UserField(this, isStatic, name);
            
            userFields.put(name, f);
        } else {
            if (f.isStatic() != isStatic) {
                Globals.nonlocalError("User field declaration mismatch for field " + f);
            }
        }
        
        return f;
    }
    
/**
Retrieves the direct superinterfaces of this class.

@return the superinterfaces
*/
    public JBCClass[] getSuperInterfaces() {
        return superInterfaces;
    }
    
    private void setupMethods() {
        MethodData[] methodData = getData().getMethods();
        JBCMethod[] methodList = new JBCMethod[methodData.length];
        
        for (int i = 0; i < methodData.length; i++) {
            JBCMethod method = new JBCMethod(this, methodData[i]);
            
            if (method.getMethodName().equals("<clinit>")) {
                if (initializerMethod != null) {
                    throw new InvalidClassDataError("Multiple <clinit> methods");
                } else {
                    initializerMethod = method;
                }
            }
            
            methodList[i] = method;
        }
        
        methods = new CompactSet(methodList.length, new ArrayEnumerator(methodList));
   }
   
   private void setupFields() {
        FieldData[] fieldData = getData().getFields();
        JBCField[] fieldList = new JBCField[fieldData.length];
        
        for (int i = 0; i < fieldData.length; i++) {
            fieldList[i] = new JBCField(this, fieldData[i]);
        }
        
        fields = fieldList.length > 0
            ? new CompactSet(fieldList.length, new ArrayEnumerator(fieldList))
            : null;
   }

/**
Retrieves the method with the given name and type declared in this class.
The type is given in
the standard Java bytecode type encoding, but with dots instead of slashes
in class names.

@return the method, or null if it does not exist
*/
    public JBCMethod getMethod(String name, String type) {
        return getMethod(new JBCMethod(this, name, type));
    }
    
/**
Find a method declared in this class that matches the name and type of m.
m's class is ignored.

@param m the method to match
@return the corresponding method in this class
*/
    public JBCMethod getMethod(JBCMethod m) {
        return m.lookupMethodInClass(this, methods);
    }
    
/**
Get a list of all the methods declared in this class.
This will include generated methods such as <init> and <clinit>.

@return an Enumeration of all the methods
*/
    public Enumeration getMethods() {
        return methods.elements();
    }
    
/**
Get the class initializer method. This is the method named <clinit>.

@return the class initializer method
*/
    public JBCMethod getInitializerMethod() {
        return initializerMethod;
    }
    
    public Enumeration getInheritedMethods() {
        CompactSet result = new CompactSet();
        
        for (JBCClass c = this; c != null; c = c.getSuperClass()) {
            for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e.nextElement();
                        
                if (m.isStatic()) {
                    if (c == this) {
                        result.add(m);
                    }
                } else {
                    JBCMethod inherited = getInheritedMethod(m);
                            
                    if (inherited != null) {
                        result.add(inherited);
                    }
                }
            }
        }
        
        return result.elements();
    }
    
    public JBCMethod getInheritedMethod(String name, String type) {
        return getInheritedMethod(new JBCMethod(this, name, type));
    }
    
    private void checkAllMethodsImplemented(JBCClass c, CompactSet visited) {
        if (visited.get(c) == null) {
            visited.addUnconditionally(c);
        
            for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
                JBCMethod m = (JBCMethod)e.nextElement();
                
                if (m.isAbstract()) {
                    JBCMethod myMethod = getImplementationMethod(m);
                
                    if (myMethod == null || myMethod.isAbstract()) {
                        throw new InvalidClassDataError("Abstract method " 
                            + m + " of non-abstract class "
                            + getClassName() + " not implemented");
                    }
                }
            }
            
            JBCClass superClass = c.getSuperClass();
            
            if (superClass != null) {
                checkAllMethodsImplemented(superClass, visited);
            }
            
            JBCClass[] superInterfaces = c.getSuperInterfaces();
            
            for (int i = 0; i < superInterfaces.length; i++) {
                checkAllMethodsImplemented(superInterfaces[i], visited);
            }
        }
    }
    
    public JBCMethod getInheritedMethod(JBCMethod m) {
        JBCMethod method = getMethod(m);
        
        if (method != null) {
            return method;
        } else {
            return getSuperInheritedMethod(m);
        }
    }
    
    public JBCMethod getImplementationMethod(JBCMethod m) {
        for (JBCClass c = this; c != null; c = c.getSuperClass()) {
            JBCMethod method = c.getMethod(m);
            
            if (method != null) {
                int flags = method.getData().getAccessFlags();
            
                if ((flags & ACC_STATIC) == 0 && m.isAccessibleTo(c)) {
                    return method;
                }
            }
        }
        
        return null;
    }
    
    public JBCMethod getSuperInheritedMethod(JBCMethod m) {
        for (JBCClass c = getSuperClass(); c != null; c = c.getSuperClass()) {
            JBCMethod method = c.getMethod(m);
            
            if (method != null) {
                int flags = method.getData().getAccessFlags();
            
                if ((flags & ACC_STATIC) == 0 && method.isAccessibleTo(this)) {
                    return method;
                }
            }
        }
        
        return null;
    }
    
    public JBCField getField(String name) {
        return getField(new JBCField(this, name));
    }
    
    public Enumeration getFields() {
        return fields == null ? new EmptyEnumerator() : fields.elements();
    }
    
    public Enumeration getInheritedFields() {
        return new InheritedFieldEnumerator(this);
    }
    
    public JBCField getField(JBCField f) {
        return fields == null ? null : f.lookupFieldInClass(this, fields);
    }
    
    void updateData(ClassData newData) {
        ClassData oldData = data;
        
        for (Enumeration e = methods.elements(); e.hasMoreElements();) {
            ((JBCMethod)e.nextElement()).setObsolete();
        }
        
        if (fields != null) {
            for (Enumeration e = fields.elements(); e.hasMoreElements();) {
                ((JBCField)e.nextElement()).setObsolete();
            }
        }
        
        init();
        
        loader.getWorld().notifyClassChanged(this, oldData, newData);
    }
    
    public String getPackageName() {
        String className = getData().getClassName();
        int lastDot = className.lastIndexOf('.');
        
        if (lastDot < 0) {
            return "";
        } else {
            return className.substring(0, lastDot);
        }
    }
    
    public boolean isSamePackage(JBCClass c) {
        if (!c.getClassLoader().equals(getClassLoader())) {
            return false;
        } else {
            return getPackageName().equals(c.getPackageName());
        }
    }
    
    private boolean isSubclassOfInterface(JBCClass c, CompactSet visited) {
        if (visited.get(this) != null) {
            return false;
        } else if (this.equals(c)) {
            return true;
        } else {
            visited.addUnconditionally(this);
            
            JBCClass superClass = getSuperClass();
            
            if (superClass != null && superClass.isSubclassOfInterface(c, visited)) {
                return true;
            } else {
                JBCClass[] superInterfaces = getSuperInterfaces();
                
                for (int i = 0; i < superInterfaces.length; i++) {
                    if (superInterfaces[i].isSubclassOfInterface(c, visited)) {
                        return true;
                    }
                }
                
                return false;
            }
        }
    }
    
    public boolean isInterface() {
        return (getData().getAccessFlags() & ACC_INTERFACE) != 0;
    }
    
    public boolean isFinal() {
        return (getData().getAccessFlags() & ACC_FINAL) != 0;
    }
    
    public boolean isPublic() {
        return (getData().getAccessFlags() & ACC_PUBLIC) != 0;
    }
    
    public boolean isAbstract() {
        return (getData().getAccessFlags() & ACC_ABSTRACT) != 0;
    }
    
    public boolean isArray() {
        return getClassName().charAt(0) == '[';
    }
    
    public JBCClass getArrayOf() {
        if (isArray()) {
            return getClassLoader().getClass("[" + getClassName());
        } else {
            return getClassLoader().getClass("[L" + getClassName() + ";");
        }
    }
    
    public JBCClass getArrayComponentClass() {
        if (isArray()) {
            JBCType t = getClassType().getArrayElementType();
            
            if (t instanceof JBCObjectType) {
                return ((JBCObjectType)t).getClassDef();
            } else {
                return null;
            }
        } else {
            return null;
        }
    }
    
    public boolean isSubclassOf(JBCClass c) {
        if ((c.getData().getAccessFlags() & ACC_INTERFACE) == 0) {
            JBCClass cl;
            
            for (cl = this; cl != null && !cl.equals(c);
                cl = cl.getSuperClass()) {
            }
            
            return cl != null;
        } else {
            return isSubclassOfInterface(c, new CompactSet());
        }
    }

    public boolean hasCommonSubclassWith(JBCClass c) {
        if (isInterface() || c.isInterface()) {
            return true;
        } else {
            return isSubclassOf(c) || c.isSubclassOf(this);
        }
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        
        if (isInterface()) {
            buf.append("interface ");
        } else {
            buf.append("class ");
        }
        
        buf.append(JBCObjectType.convertClassNameToHumanReadable(getClassName()));
        
        JBCClassLoader loader = getClassLoader();
        
        if (!loader.equals(loader.getWorld().getSystemClassLoader())) {
            buf.append(" (classloader " + loader.toString() + ")");
        }
        
        return buf.toString();
    }
}
