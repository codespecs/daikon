/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.jbc.util;

import ajax.jbc.*;
import java.util.*;
import ajax.util.*;

public class JBCParserUtilities {
    private static boolean numParamsMatch(JBCMethod m, int numParams, boolean adjustNonstaticParams) {
        if (numParams < 0) {
            return true;
        } else {
            int trueParams = m.getMethodType().getParameterTypes().length;
            
            if (adjustNonstaticParams && !m.isStatic()) {
                trueParams--;
            }
            
            return trueParams == numParams;
        }
    }
    
    public static JBCMethod findMethod(JBCClass c, String methodName,
        String signature, int numParams, boolean adjustNonstaticParams)
        throws MissingMethodException, AmbiguousMethodException {
        JBCMethod found = null;
        
        for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            
            if (m.getMethodName().equals(methodName)
                && (signature == null || m.getMethodTypeName().startsWith(signature))
                && numParamsMatch(m, numParams, adjustNonstaticParams)) {
                if (found != null) {
                    throw new AmbiguousMethodException("Method " + methodName + " ambiguous in " + c, found);
                } else {
                    found = m;
                }
            }
        }
        
        if (found == null) {
            String sig = signature;
            
            if (sig == null) {
                if (numParams >= 0) {
                    sig = "(" + numParams + " params)";
                } else {
                    sig = "(unknown signature)";
                }
            }
            
            throw new MissingMethodException("Method " + methodName
                + sig + " not found in " + c.getClassName(), c.getClassName(), methodName);
        }
        
        return found;
    }
    
    public static JBCMethod findMethod(JBCClassLoader classLoader, String name,
        String signature, int numParams, boolean adjustNonstaticParams)
        throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        int lastDot = name.lastIndexOf('.');
        
        if (lastDot < 0) {
            return null;
        }
        
        String className = name.substring(0, lastDot);
        String methodName = name.substring(lastDot + 1);
        JBCClass c = classLoader.getClass(className);

        if (c == null) {
            throw new UnresolvedClassException(className, "Class not found: " + className);
        }
        
        return findMethod(c, methodName, signature, numParams, adjustNonstaticParams);
    }
    
    public static CompactSet parseFunctionSet(JBCClassLoader loader, String list)
        throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        CompactSet result = new CompactSet();
        
        while (list != null) {
            int lastComma = list.indexOf(',');
            String fun;
            
            if (lastComma < 0) {
                fun = list;
                list = null;
            } else {
                fun = list.substring(0, lastComma);
                list = list.substring(lastComma + 1);
            }
            
            result.add(JBCParserUtilities.findMethod(loader, fun));
        }
        
        return result;
    }
    
    public static JBCMethod findMethod(JBCClassLoader loader, String name)
        throws UnresolvedClassException, MissingMethodException, AmbiguousMethodException {
        int lastParen = name.lastIndexOf('(');
        int paramCount = -1;
        StringBuffer signature = null;
        
        if (lastParen >= 0 && name.endsWith(")")) {
            String args = name.substring(lastParen + 1, name.length() - 1);
            
            paramCount = 0;
            signature = new StringBuffer("(");
            
            while (args != null) {
                int index = args.indexOf(',');
                
                paramCount++;
                
                if (signature != null) {
                    String type = JBCType.convertHumanReadableTypeToTypeCode(
                        index >= 0 ? args.substring(0, index) : args);
                        
                    signature.append(type);
                }
                
                if (index >= 0) {
                    args = args.substring(index + 1);
                } else {
                    args = null;
                }
            }
            
            signature.append(")");
            
            name = name.substring(0, lastParen);
        }
            
        return findMethod(loader, name, signature == null ? null : signature.toString(), paramCount, true);
    }
}
