/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import ajax.Globals;
import java.io.*;
import java.util.*;

public class StandardClassLoader extends JBCClassLoader {
    private ClassReader[] readers;
    private JBCClassLoader parentClassLoader;
    
    public StandardClassLoader(JBCWorld world) {
        this(world, getDefaultClassPath());
    }
    
    public StandardClassLoader(JBCWorld world, String classPath) {
        this(world, parseClassPath(classPath));
    }
    
    public StandardClassLoader(JBCWorld world, ClassReader[] readers) {
        super(world);
        
        this.readers = readers;
        this.parentClassLoader = null;
    }
    
    public void setParentClassLoader(JBCClassLoader parentClassLoader) {
        if (this.parentClassLoader != null) {
            Globals.nonlocalError("Changing parent class loader");
        }
        
        this.parentClassLoader = parentClassLoader;
    }
    
    public static ClassReader[] parseClassPath(String path) {
        Vector readers = new Vector();
        
        if (path == null) {
            Globals.nonlocalError("Empty class path not allowed");
        }
        
        while (path != null) {
            int separator = path.indexOf(File.pathSeparatorChar);
            String name;
            
            if (separator < 0) {
                name = path;
                path = null;
            } else {
                name = path.substring(0, separator);
                path = path.substring(separator + 1);
            }
            
	    if (name.startsWith("#")) {
		
	    } else {
		try {
		    File file = new File(name);
                
		    if (file.exists()) {
			if (name.regionMatches(true, name.length() - 4, ".zip", 0, 4)
			    || name.regionMatches(true, name.length() - 4, ".jar", 0, 4)) {
			    readers.addElement(new ZipClassReader(file));
			} else if (file.isDirectory()) {
			    readers.addElement(new FileSystemClassReader(file));
			} else {
                            Globals.userError("Class path entry " + name + " is not a .zip or .jar, "
					      + "nor a directory; ignoring");
			}
		    } else {
			Globals.userError("Class path entry " + name + " not found");
		    }
		} catch (IOException ex) {
		    Globals.userError("Can't read class path entry: "
				      + name + " (" + ex.getMessage() + "); ignoring");
		}
	    }
        }
        
        ClassReader[] result = new ClassReader[readers.size()];

        readers.copyInto(result);
        
        if (result.length == 0) {
            Globals.userError("No valid classpath entries found");
        }
        
        return result;
    }
    
    public static String getDefaultClassPath() {
        String javaClassPath = System.getProperty("java.class.path");
        String sunBootClassPath = System.getProperty("sun.boot.class.path");
        
        if (sunBootClassPath != null) {
            return javaClassPath + File.pathSeparatorChar + sunBootClassPath;
        } else {
            return javaClassPath;
        }
    }
    
    public Enumeration getClassList() {
        return new StandardClassLoaderClassList(readers);
    }
    
    public ClassData loadClassData(String fullName) throws InvalidClassDataError {
        for (int i = 0; i < readers.length; i++) {
            byte[] classData = readers[i].readClass(fullName);

        if (classData != null) {
        return new ClassFileParser(classData);
        }
        }
        
        return null;
    }

    public String loadClassSource(String className, String fileName) {
        if (fileName == null) {
            fileName = getDefaultFileName(className);
        }

        for (int i = 0; i < readers.length; i++) {
            String source = readers[i].readSource(className, fileName);

            if (source != null) {
                return source;
            }
        }

        return null;
    }
    
    protected JBCClass findClass(String fullName, boolean caseSensitive) {
        JBCClass c = super.findClass(fullName, caseSensitive);
        
        if (c == null && parentClassLoader != null) {
            return parentClassLoader.getClass(fullName, caseSensitive);
        } else {
            return c;
        }
    }
    
    public String toString() {
        StringBuffer buf = new StringBuffer();
        
        buf.append("StandardClassLoader[");
        
        for (int i = 0; i < readers.length; i++) {
            if (i > 0) {
                buf.append(", ");
            }
            buf.append(readers[i].toString());
        }
        
        buf.append("]");
        
        if (parentClassLoader != null) {
            buf.append("[parent = ").append(parentClassLoader.toString())
                .append("]");
        }
        
        return buf.toString();
    }
}
