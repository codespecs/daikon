/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.Globals;
import ajax.solver.*;
import java.util.Hashtable;

/**
This class is the static analogue of a Java class loader object. It manages a class
namespace and also loads class data from some source. Unlike the Java class loader,
however, the class data need not be in the class file format --- although it usually
will be.
<p>
Class names used in this class loader follow the standard used by Java's Class.getName
method. That means that a non-array class has the standard, dot-separated, fully qualified
name. The name of an array class is determined in a few possible ways:
<ul>
<li>If the element is also an array class, then the name of the containing array class is "["
+ the name of the element.</li>
<li>If the element is some non-array class, then the name of the containing array class is
"[L" + the name of the element + ";".</li>
<li>If the element is some non-class type, then the name of the array class is
"[B", "[Z", "[C", "[S", "[I", "[F", "[J", or "[D", depending on the type.</li>
</ul>
<p>
Note that all class names are fully qualified and dot separated. The silly slash
separators are never used in any Ajax code --- we convert them immediately as soon
as they are read from a class file.
<p>
The array classes for the base types are sourced from the system class loader.
Other array classes are always sourced from the classloader that is the source of
the element type. Note that class data for array classes should never be loaded;
this data is fabricated on demand by JBCWorld and JBCClassLoader. Therefore the handling
of array classes should be fairly transparent to the subclasses of this class.
<p>
See StandardClassLoader for a useful and simple fleshing-out of this class.
'loadClassData' must be overriden to provide access to class data for this class loader.
'findClassData' may be overriden to provide some fallback handling if the data cannot
be loaded or it is corrupt. For example, StandardClassLoader overrides it to search a
parent class loader if the current loader cannot load the class. JBCClassLoader.findClassData
calls loadClassData; that is the only way for a ClassData to be converted into a JBCClass.

@see ajax.jbc.JBCWorld#setSystemClassLoader
@see ajax.jbc.util.StandardClassLoader
@see ajax.jbc.util.ClassFileParser
*/
public abstract class JBCClassLoader {
    private Hashtable classCache = new Hashtable();
    private JBCWorld world;
    private JBCClassLoader parent = null;
    
    private static final Object NOT_FOUND = new StringBuffer("NOT_FOUND");
    private static final Object LOADING = new StringBuffer("LOADING");

/**
Create a class loader and attach it to a JBCWorld.

@param world the program this class loader is for
*/
    public JBCClassLoader(JBCWorld world) {
        this.world = world;
    }
    
    public JBCWorld getWorld() {
        return world;
    }
    
/**
This method allows you to set a parent class loader for this class loader.
A class unresolved by this class loader is passed to the parent for
resolution. If the parent finds the class, then the class is returned;
note that in this case, the return class's class loader will be the parent
and not this class loader.
<p>
Only one parent can be set. Adding another parent will override the current
setting.

@param parent the class loader to become the parent
*/
    public void setParent(JBCClassLoader parent) {
        this.parent = parent;
    }
    
    public JBCClassLoader getParent() {
        return parent;
    }
    
/**
Override this method to determine how class data is actually loaded.
This method must ensure that the returned class data is actually for the right
class. 

@param fullName the fully qualified name (with dots) of the class to load
@return the class data loaded, or null if no data can be found
@throws ajax.jbc.InvalidClassDataError
The classloader thinks it can load the class, but the data is bad
*/
    abstract public ClassData loadClassData(String fullName) throws InvalidClassDataError;

/**
Override this method to determine how source code is actually loaded.

@param className the fully qualified name (with dots) of the class
@param fileName the name of the source file
@return the source, or null if it's not available
*/
    abstract public String loadClassSource(String className, String fileName);

    protected static String getDefaultFileName(String className) {
	int lastDot = className.lastIndexOf('.');

	if (lastDot < 0) {
	    return className + ".java";
	} else {
	    return className.substring(lastDot + 1) + ".java";
	}
    }
    
/**
Get the JBCClass object corresponding to a particular class in this class loader's
view of the world. Normally this will not be overriden. It maintains a cache; if the
cache cannot answer the request, the request is passed to findClassData.

@param fullName the name of the class, as described in the introduction
@return the JBCClass object, or null if no class can be found
*/
    public JBCClass getClass(String fullName) {
        return getClass(fullName, true);
    }
    
    public JBCClass getClass(String fullName, boolean caseSensitive) {
        Object c = classCache.get(fullName);
        
        if (c != null) {
            if (NOT_FOUND.equals(c)) {
                return null;
            } else if (LOADING.equals(c)) {
                Globals.userError("Circular class hierarchy detected involving class " + fullName);
                return null;
            } else {
                return (JBCClass)c;
            }
        } else {
            classCache.put(fullName, LOADING);
            
            JBCClass foundClass = findClass(fullName, caseSensitive);
            
            if (foundClass != null) {
                classCache.put(fullName, foundClass);
            } else {
                classCache.put(fullName, NOT_FOUND);
            }
            
            return foundClass;
        }
    }
    
    public JBCClass getClassCaseInsensitive(String fullName) {
        return getClass(fullName, false);
    }
    
/**
This method is used by JBCWorld to stuff a class into the cache. It may
replace a class already loaded; if so, then we try to handle it.
This method can also be used by a class loader to indicate that the
class has changed.
<p>
NOTE: class mutation is NOT really supported, yet.

@param classData data for the class that's being acquired
@return the loaded class
*/
    protected JBCClass setClass(ClassData classData) {
        String fullName = classData.getClassName();
        Object c = classCache.get(fullName);
        
        if (c instanceof JBCClass) {
            JBCClass theClass = (JBCClass)c;
            
            theClass.updateData(classData);
            return theClass;
        } else {
            JBCClass foundClass = new JBCClass(this, classData);
            
            classCache.put(fullName, foundClass);
            
            return foundClass;
        }
    }
    
/**
Attempt to locate and load the given class.

@param fullName the class name, as described above
@param caseSensitive true if the class name must match the case of the given name
@return the loaded class object, or null if it cannot be found or it is corrupt
*/
    protected JBCClass findClass(String fullName, boolean caseSensitive) {
        try {
            if (fullName.startsWith("[")) {
                String elementClassName = JBCObjectType.convertTypeCodeToClassName(
                    fullName.substring(1));
                
                if (elementClassName != null) {
                    JBCClass elementClass = getClass(elementClassName, caseSensitive);
                    
                    if (elementClass == null) {
                        return null;
                    } else {
                        JBCClassLoader elementLoader = elementClass.getClassLoader();

                        if (elementLoader == this) {
                            return setClass(new JBCArrayClassData(fullName));
                        } else {
                            return elementLoader.getClass(fullName, caseSensitive);
                        }
                    }
                } else if (getWorld().getSystemClassLoader() == this) {
                    return null;
                } else {
                    return getWorld().getSpecialClass(fullName);
                }
            } else {
                ClassData classData = loadClassData(fullName);
                
                if (classData == null) {
                    if (parent == null) {
                        return null;
                    } else {
                        return parent.getClass(fullName, caseSensitive);
                    }
                } else {
                    if (!(caseSensitive ? fullName.equals(classData.getClassName())
                        : fullName.equalsIgnoreCase(classData.getClassName()))) {
                        Globals.nonlocalError("Class name mismatch: expected " +
                            fullName + ", got " + classData.getClassName());

                        return null;
                    }
                    
                    return new JBCClass(this, classData);
                }
            }
        } catch (InvalidClassDataError ex) {
            Globals.userError("Invalid class data loading class " + fullName
                + " (" + ex.getMessage() + ")");

            return null;
        }
    }
}
