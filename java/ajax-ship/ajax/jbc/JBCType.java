/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc;

import java.io.*;
import ajax.Globals;
import ajax.util.IdentityManager;
import java.util.*;

/**
This is the base class of the classes that represent JBC types.
*/
abstract public class JBCType implements DataConstants {
    public static final JBCType RETURNADDR = new JBCBaseType("<ret>");
    public static final JBCType VOID = new JBCBaseType("void", 0);
    public static final JBCType BOOLEAN = new JBCBaseType("boolean", true);
    public static final JBCType BYTE = new JBCBaseType("byte", true);
    public static final JBCType CHAR = new JBCBaseType("char", true);
    public static final JBCType SHORT = new JBCBaseType("short", true);
    public static final JBCType INT = new JBCBaseType("int", true);
    public static final JBCType LONG = new JBCBaseType("long", 2);
    public static final JBCType OBJECT = new JBCObjectType();
    public static final JBCType FLOAT = new JBCBaseType("float");
    public static final JBCType DOUBLE = new JBCBaseType("double", 2);

    private static final Hashtable humanReadableElementTypes
        = makeHumanReadableElementTypes();
    
    private static Hashtable makeHumanReadableElementTypes() {
        Hashtable result = new Hashtable();
        
        result.put("void", "V");
        result.put("int", "I");
        result.put("float", "F");
        result.put("long", "J");
        result.put("boolean", "Z");
        result.put("char", "C");
        result.put("short", "S");
        result.put("double", "D");
        result.put("byte", "B");
        
        return result;
    }
    
    public static String getPrimitiveTypeCode(String s) {
        return (String)humanReadableElementTypes.get(s);
    }
    
    public int getWordSize() {
        Globals.nonlocalError("Type has undefined word size");
        return 0;
    }
  
/**
This method provides a "weak" test for type equality. With this weak test,
boolean, byte, char, short, and int are all equal. Furthermore, all object
types are equal.

@param t the type to compare this to
@return true if t is "weakly" equal to this
*/
    public abstract boolean isEqualType(JBCType t);
    
    public static JBCType parseType(String s) {
        return parseType(s, null);
    }
    
/**
This function computes the internal Java type code from a human-readable
type.
*/
    public static String convertHumanReadableTypeToTypeCode(String s) {
        String t = getPrimitiveTypeCode(s);
        
        if (t != null) {
            return t;
        } else {
            return JBCObjectType.convertClassNameToTypeCode(
                JBCObjectType.convertHumanReadableToClassName(s));
        }
    }
    
    public static JBCType parseType(String s, JBCClassLoader loader) {
        try {
            switch (s.charAt(0)) {
                case 'V': return VOID;
                case 'B': return BYTE;
                case 'C': return CHAR;
                case 'D': return DOUBLE;
                case 'F': return FLOAT;
                case 'I': return INT;
                case 'J': return LONG;
                case 'S': return SHORT;
                case 'Z': return BOOLEAN;
                
                case 'L':
                case '[': {
                    String className = JBCObjectType.convertTypeCodeToClassName(s);
                    
                    if (className == null) {
                        throw new InvalidClassDataError("Unknown type: " + s);
                    } else {
                        return JBCObjectType.get(className, loader);
                    }
                }
                    
                default:
                    throw new InvalidClassDataError("Unknown type: " + s);
            }
        } catch (StringIndexOutOfBoundsException ex) {
            throw new InvalidClassDataError("Unknown type: " + s);
        }
    }
    
    public static int getMethodParamsSize(String s) {
        StringReader r = new StringReader(s);
        int result = 0;
        
        try {
            if (r.read() == '(') {
                JBCType t;
                
                while ((t = parseType(r)) != null) {
                    if (t.equals(VOID)) {
                    } else if (t.equals(DOUBLE) || t.equals(LONG)) {
                        result += 2;
                    } else {
                        result += 1;
                    }
                }
                
                if (r.read() == ')') {
                    return result;
                }
            }
        } catch (IOException ex) {
        }
        
        throw new InvalidClassDataError("Invalid method type signature: " + s);
    }
    
    public static int getMethodResultSize(String s) {
        int paramsEnd = s.indexOf(')');
        
        if (paramsEnd > 0) {
            return getWordSize(s.substring(paramsEnd + 1));
        }
        
        throw new InvalidClassDataError("Invalid method type signature: " + s);
    }
    
    public static int getWordSize(String s) {
        try {
            switch (s.charAt(0)) {
                case 'V': return 0;
                case 'B': return 1;
                case 'C': return 1;
                case 'D': return 2;
                case 'F': return 1;
                case 'I': return 1;
                case 'J': return 2;
                case 'S': return 1;
                case 'Z': return 1;
                case 'L': return 1;
                case '[': return 1;
                default:
                    throw new InvalidClassDataError("Unknown type: " + s);
            }
        } catch (StringIndexOutOfBoundsException ex) {
            throw new InvalidClassDataError("Unknown type: " + s);
        }
    }
    
    public static JBCType parseType(StringReader r) throws IOException {
        return parseType(r, null);
    }
    
    public static JBCType parseType(StringReader r, JBCClassLoader loader) throws IOException {
        r.mark(Integer.MAX_VALUE);
        
        int ch = r.read();
        
        if (ch == -1) {
            return null;
        } else {
            boolean isArray = false;
            JBCType type;
            StringBuffer buf = new StringBuffer();
            
            buf.append((char)ch);
            
            while ((char)ch == '[') {
                ch = r.read();
                isArray = true;
                
                if (ch != -1) {
                    buf.append((char)ch);
                }
            }
            
            switch ((char)ch) {
                case 'V': type = VOID; break;
                case 'B': type = BYTE; break;
                case 'C': type = CHAR; break;
                case 'D': type = DOUBLE; break;
                case 'F': type = FLOAT; break;
                case 'I': type = INT; break;
                case 'J': type = LONG; break;
                case 'S': type = SHORT; break;
                case 'Z': type = BOOLEAN; break;
                case 'L': {
                    do {
                        ch = r.read();
                        if (ch != -1) {
                            buf.append((char)ch);
                        }
                    } while ((char)ch != ';' && ch != -1);
                    
                    if ((char)ch == ';') {
                        String className = JBCObjectType.convertTypeCodeToClassName(buf.toString());
                        
                        if (className == null) {
                            r.reset();
                            return null;
                        } else {
                            /* this could be an array, but we'll get the right answer anyhow */
                            return JBCObjectType.get(className, loader);
                        }
                    } else {
                        r.reset();
                        return null;
                    }
                }
                
                default:
                    r.reset();
                    return null;
            }
            
            if (isArray) {
                /* for primitive arrays, the class name is as given */
                return JBCObjectType.get(buf.toString(), loader);
            } else {
                return type;
            }
        }       
    }

    public static String getBasicArrayClassName(int type) {
        switch (type) {
            case T_BOOLEAN: return "[Z";
            case T_CHAR:    return "[C";
            case T_FLOAT:   return "[F";
            case T_DOUBLE:  return "[D";
            case T_SHORT:   return "[S";
            case T_BYTE:    return "[B";
            case T_INT:     return "[I";
            case T_LONG:    return "[J";
            default:        return null;
        }
    }
    
    public static String getObjectArrayClassName(String className) {
        if (className == null) {
            return null;
        } else if (className.startsWith("[")) {
            return "[" + className;
        } else {
            return "[L" + className + ";";
        }
    }
    
    public int hashCode() {
        return IdentityManager.getIdentityHashCode(this);
    }
}
