/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import ajax.Globals;
import ajax.util.*;
import java.util.*;

/**
This class represents JBC class types, including arrays.

All "class names" used here are in the format described in the introduction to
JBCClassLoader. There are some conversion functions here to help deal with other formats.
*/
public class JBCObjectType extends JBCType {
    private static WeakSet cache = new WeakSet();
    
    private String name;
    private JBCClassLoader loader;
    
    public static JBCObjectType get(String name, JBCClassLoader loader) {
        if (loader != null) {
            return (JBCObjectType)cache.add(new JBCObjectType(name, loader));
        } else {
            return new JBCObjectType(name);
        }
    }
    
    public static JBCObjectType get(JBCClass c) {
        return get(c.getClassName(), c.getClassLoader());
    }

/**
Construct a JBCObjectType with the given class name and loader.

@param name the name of the class
@param loader the class loader
*/
    private JBCObjectType(String name, JBCClassLoader loader) {
        this.name = name;
        this.loader = loader;
    }

/**
Construct a JBCObjectType with the given class name and no loader.
The resulting JBCObjectType is not usable except for being converted
to a String.

@param name the name of the class
*/
    private JBCObjectType(String name) {
        this(name, null);
    }

/**
Construct a JBCObjectType for the type of all objects (class unknown).
*/
    JBCObjectType() {
        this(null, null);
    }
    
    public int getWordSize() {
        return 1;
    }
    
    public JBCClass getClassDef() {
        if (Globals.debug && (loader == null || name == null)) {
            Globals.nonlocalError("Attempting to get the JBCClass of an unspecified class or class loader");
        }
        
        return loader.getClass(name);
    }
    
    public String getClassName() {
        return name;
    }
    
    public boolean isEqualType(JBCType t) {
        return t instanceof JBCObjectType;
    }
    
    public JBCType getArrayElementType() {
        if (name != null && name.charAt(0) == '[') {
            return JBCType.parseType(name.substring(1), loader);
        } else {
            return null;
        }
    }
    
/**
This checks whether the names and *initiating* class loaders are
equal. In some cases, this will return false even when the resolved
class would be the same.
*/
    public boolean equals(Object o) {
        if (o instanceof JBCObjectType) {
            JBCObjectType t = (JBCObjectType)o;
            
            return name == null ? t.name == null
                : name.equals(t.name) && loader.equals(t.loader);
        } else {
            return false;
        }
    }
    
/**
See comments for "equals" above. In some cases, this will return different
hash values even when the resolved class would be the same.
*/
    public int hashCode() {
        return name == null ? 1883418
            : 1849817*name.hashCode() + 18831 + 7677731*loader.hashCode();
    }
    
    public String toString() {
        return convertClassNameToHumanReadable(name);
    }
    
    public static String convertTypeCodeToClassName(String typeCode) {
        int len = typeCode.length();
        
        if (len == 0) {
            return null;
        } else {
            char ch0 = typeCode.charAt(0);
            
            if (ch0 == '[') {
                return typeCode;
            } else if (ch0 == 'L' && typeCode.charAt(len - 1) == ';') {
                return typeCode.substring(1, len - 1);
            } else {
                return null;
            }
        }
    }
    
    public static String convertClassNameToTypeCode(String s) {
        int len = s.length();
        
        if (len == 0) {
            return null;
        } else {
            char ch0 = s.charAt(0);
            
            if (ch0 == '[') {
                return s;
            } else {
                return "L" + s + ";";
            }
        }
    }
    
    private static String convertElementType(String s) {
        String internalType = JBCType.getPrimitiveTypeCode(s);
        
        if (internalType != null) {
            return internalType;
        } else {
            return s;
        }
    }
    
/**
This function computes the internal Java type code from a human-readable
type. It works for any type, not just classes.
*/
    public static String convertHumanReadableToClassName(String s) {
        if (s == null) {
            return "<unknown>";
        } else if (!s.endsWith("[]")) {
            return s;
        } else {
            int arrayIndices = 1;
            int sLen = s.length();
                
            while (arrayIndices*2 + 2 < sLen
                && s.substring(sLen - arrayIndices*2 - 2).equals("[]")) {
                arrayIndices++;
            }
                
            String elementType = convertElementType(s.substring(0, sLen - arrayIndices*2));
            StringBuffer result = new StringBuffer(arrayIndices + elementType.length());
                    
            for (int i = 0; i < arrayIndices; i++) {
                result.append('[');
            }
            result.append(elementType);
                    
            return result.toString();
        }
    }
    
    public static String convertClassNameToHumanReadable(String s) {
        if (s == null) {
            return "<anyobject>";
        } else if (!s.startsWith("[")) {
            return s;
        } else {
            int sLen = s.length();
            int arrayIndices = 1;
            int index = 1;
                
            while (index < sLen && s.charAt(index) == '[') {
                index++;
                arrayIndices++;
            }
                
            String elementName = JBCType.parseType(s.substring(index)).toString();
                
            if (elementName == null) {
                return null;
            } else {
                StringBuffer buf = new StringBuffer(elementName.length() + 2*arrayIndices);
                    
                buf.append(elementName);
                for (int i = 0; i < arrayIndices; i++) {
                    buf.append("[]");
                }
                return buf.toString();
            }
        }
    }
}
