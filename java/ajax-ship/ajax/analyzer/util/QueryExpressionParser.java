/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.analyzer.util;

import ajax.jbc.*;
import ajax.jbc.util.*;
import ajax.util.*;
import ajax.analyzer.*;
import java.util.*;

/**
Format for query expressions:
    program-point::
        <fully-qualified-class-name> . <method-name> # <bytecode-index>

    expression::
        local-<n>
        stack-<n>
        <static-field>
        <expression> , <field-name>
        <expression> []
        
    static-field::
        <fully-qualified-class-name> . <field-name>

    query-expression::
        <program-point> : <expression>


NEW FORMAT:

    program-point::
        <method>
        <method> # <bytecode-index-number>
        <method> @ <source-line-number>

    method::
        <class> . <method-name>
        <method-name>

    expression::
        local-<n>
        stack-<n>
        <local-variable-name>
        <static-field-name>
        <expression> . <field-name>
        <expression> []
        
    static-field::
        <class> . <field-name>
        <field-name>

    class::
        <class-name>
        <package> . <class-name>

    package::
        <package-name>
        <package> . <package-name>

    query-expression::
        <program-point> : <expression>
*/
public class QueryExpressionParser {
    public static final int ALLOW_NO_INDEX = 0x01;
    
    private QueryExpressionParser() {
    }
    
    public static JBCLocation parseLocation(JBCClassLoader env, String s) throws ParseException {
        return parseLocation(env, s, 0);
    }
    
    public static JBCLocation parseLocation(JBCClassLoader env, String s, int flags) throws ParseException {
        CompactSet results = parseLocationSet(env, s, flags);
        if (results.size() > 1) {
            throw new ParseException("Expression is ambiguous");
	}
        if (results.size() == 0) {
            return null;
	} else {
            return (JBCLocation)results.elements().nextElement();
	}
    }

    public static CompactSet findMethods(JBCClassLoader env, String name) throws ParseException {
        CompactSet results = new CompactSet();

        String classPart;
        String methodName;
        int lastDot = name.lastIndexOf('.');
        if (lastDot >= 0) {
            classPart = name.substring(0, lastDot);
            methodName = name.substring(lastDot + 1);
	} else {
            classPart = null;
            methodName = name;
	}

        if (env instanceof StandardClassLoader) {
            String lastClassName = null;
	    for (Enumeration e = ((StandardClassLoader)env).getClassList(); e.hasMoreElements();) {
	        String cName = (String)e.nextElement();
                if (classPart == null || classPart.equals(cName)
                    || cName.endsWith("." + classPart)) {
                    JBCClass c = env.getClass(cName);
                    if (c != null) {
                        lastClassName = cName;
                        for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                            JBCMethod m = (JBCMethod)e2.nextElement();
                            if (m.getMethodName().equals(methodName)) {
                                results.add(m);
    			    }
			}
		    }
		}
	    }

            if (lastClassName == null) {
                throw new ParseException("Class name '" + classPart + "' not found");
            }
            if (results.size() == 0) {
                throw new ParseException("No method named '" + methodName + "' found in class " + lastClassName);
	    }
	} else {
            JBCClass c = env.getClass(classPart);
                    if (c != null) {
                        for (Enumeration e2 = c.getMethods(); e2.hasMoreElements();) {
                            JBCMethod m = (JBCMethod)e2.nextElement();
                            if (m.getMethodName().equals(methodName)) {
                                results.add(m);
    			    }
			}
		    }
	}

        if (results.size() == 0) {
            throw new ParseException("Method name '" + name + "' not understood, not sure why");
	}

        return results;
    }

    public static CompactSet parseLocationSet(JBCClassLoader env, String s, int flags) throws ParseException {
        int offset = -1;
        // boolean allowNoIndex = (flags & ALLOW_NO_INDEX) != 0;
        int hashIndex = s.indexOf('#');
        int atIndex = s.indexOf('@');

        if (hashIndex >= 0 && atIndex >= 0) {
            throw new ParseException("Cannot specify both '#' and '@' in " + s);
	}
        
        int index = hashIndex >= 0 ? hashIndex : atIndex;
        String methodPart = index >= 0 ? s.substring(0, index) : s;
        
        CompactSet methods = findMethods(env, methodPart);

        if (index >= 0) {        
            String indexPart = index < 0 ? null : s.substring(index + 1);

            try {
                offset = Integer.parseInt(indexPart);
            } catch (NumberFormatException ex) {
                throw new ParseException("Invalid bytecode offset: " + indexPart);
            }
	}

        CompactSet results = new CompactSet();
        boolean allAbstract = true;
        JBCMethod lastMethod = null;
        boolean allOutOfRange = index >= 0;
        for (Enumeration e = methods.elements(); e.hasMoreElements();) {
            JBCMethod m = (JBCMethod)e.nextElement();
            lastMethod = m;
            byte[] code = m.getData().getCode();
            
            if (code != null) {
                allAbstract = false;
                if (hashIndex >= 0) {
                    if (offset >= 0 && offset < code.length) {
                        allOutOfRange = false;
                        results.add(new JBCLocation(m, offset));
                    }
		} else if (atIndex >= 0) {
                    LineNumberData[] lineData = m.getData().getLineNumbers();
                    int lastBeforePoint = -1;
                    for (int i = 0; i < lineData.length; i++) {
                        int lineNum = lineData[i].getLineNumber();

                        if (lineNum <= offset && (lastBeforePoint == -1 ||
                                lineData[lastBeforePoint].getLineNumber() < lineNum)) {
                            lastBeforePoint = i;
			}
		    }

                    if (lastBeforePoint >= 0) {
                        allOutOfRange = false;
                        int startPC = lineData[lastBeforePoint].getStartingPC();
                        int endPC = code.length;

                        for (int i = 0; i < lineData.length; i++) {
                            int PC = lineData[i].getStartingPC();

                            if (PC > startPC && PC < endPC) {
                                endPC = PC;
			    }
		        }
                        boolean[] starts = JBCCodeUtilities.getInstructionStarts(m.getData());
                        for (int i = startPC; i < endPC; i++) {
  			    if (starts[i]) {
                                results.add(new JBCLocation(m, i));
			    }
			}
		    }
		} else {
  		    boolean[] starts = JBCCodeUtilities.getInstructionStarts(m.getData());
		    for (int i = 0; i < starts.length; i++) {
		      if (starts[i]) {
			results.add(new JBCLocation(m, i));
		      }
		    }
		}
	    }
        }

        if (allAbstract) {
            throw new ParseException("Method is native or abstract: " + lastMethod);
	}
        if (allOutOfRange) {
            throw new ParseException((hashIndex >= 0 ? "Bytecode offset" : "Source line") + " is out of range: " + offset);
	}
        if (results.size() == 0) {
            throw new ParseException("Location '" + s + "' not understood, not sure why");
	}

        return results;
    }
    
    private static JBCField parseFieldExpression(JBCLocation loc, JBCClassLoader env, String s, JBCClass context) throws ParseException {
        if (context != null) {
            JBCField result = context.getField(s);
            
            if (result != null) {
                return result;
            }
        }
        
        int lastDot = s.lastIndexOf('.');
        
        if (lastDot <= 0) {
            throw new ParseException("Field name must include a class name: " + s);
        } else {
	    String className = s.substring(0, lastDot);
	    JBCClass c = env.getClass(className);
        
	    if (c == null) {
	      throw new ParseException("Class not found: " + className);
	    }
        
	    String fieldName = s.substring(lastDot + 1);
	    JBCField result = c.getField(fieldName);
        
	    if (result == null) {
	      throw new ParseException("Field " + fieldName + " not found in " + c);
	    }
        
            return result;
	}
    }
    
/**
@return a CompactSet of JBCValuePoints
*/
    private static CompactSet parseBasicExpression(CompactSet locations, JBCClassLoader env, String s) throws ParseException {
        CompactSet result = new CompactSet();
        int firstDot = s.indexOf('.');
        String fullString = s;

        if (firstDot >= 0) {
            s = s.substring(0, firstDot);
	}
        
        if (s.startsWith("local-")) {
            String index = s.substring(6);
            int i;
            
            try {
                i = Integer.parseInt(index);
            } catch (NumberFormatException ex) {
                throw new ParseException("Local index is not a number: " + index);
            }

            for (Enumeration e = locations.elements(); e.hasMoreElements();) {            
                JBCLocation loc = (JBCLocation)e.nextElement();

                if (i >= 0 && i <= loc.getMethod().getData().getMaxLocalWords()) {
                    result.add(new JBCValuePoint(loc, JBCExpression.makeLocalVarExpression(i)));
		}
	    }

            if (result.size() == 0) {
                throw new ParseException("Invalid local variable index: " + i);
	    }
        } else if (s.startsWith("stack-")) {
            String index = s.substring(6);
            int i;
            
            try {
                i = Integer.parseInt(index);
            } catch (NumberFormatException ex) {
                throw new ParseException("Stack index is not a number: " + index);
            }
            
            for (Enumeration e = locations.elements(); e.hasMoreElements();) {            
                JBCLocation loc = (JBCLocation)e.nextElement();

                if (i >= 0 && i <= loc.getMethod().getData().getMaxStackWords()) {
                    result.add(new JBCValuePoint(loc, JBCExpression.makeStackElemExpression(i)));
		}
            }
            
            if (result.size() == 0) {
                throw new ParseException("Invalid stack variable index: " + i);
	    }
        } else {
            for (Enumeration e = locations.elements(); e.hasMoreElements();) {            
                JBCLocation loc = (JBCLocation)e.nextElement();
                String[] varNames = JBCCodeUtilities.getLocalVariableNames(loc.getMethod(), loc.getOffset());

                for (int i = 0; i < varNames.length; i++) {
		    if (varNames[i].equals(s)) {
                        result.add(new JBCValuePoint(loc, JBCExpression.makeLocalVarExpression(i)));
		    }
		}
	    }
 
            if (result.size() == 0) {
                for (Enumeration e = locations.elements(); e.hasMoreElements();) {
                    JBCLocation loc = (JBCLocation)e.nextElement();
                    JBCMethod m = loc.getMethod();
                    JBCClass c = m.getContainingClass();

                    while (c != null) {
		        if (c.getField(s) != null) {
                            result.add(new JBCValuePoint(loc, JBCExpression.makeLocalVarExpression(0).makeNonstaticFieldExpression(c.getField(s))));
                            break;
		        }
                        c = c.getSuperClass();
		    }
		}
	    }

            if (result.size() == 0) {
                try {
                    for (Enumeration e = locations.elements(); e.hasMoreElements();) {
                        JBCLocation loc = (JBCLocation)e.nextElement();
                        result.add(JBCExpression.makeStaticFieldExpression(
                            parseFieldExpression(loc, env, fullString, null)));
		    }
                } catch (ParseException ex) {
                    throw new ParseException("Unknown expression: " + s);
                }
	    }
        }
        
        return result;
    }
    
/**
@return a CompactSet of JBCValuePoints
*/
    public static CompactSet parseExpression(JBCClassLoader env, String s) throws ParseException {
        int firstColon = s.indexOf(':');
        
        if (firstColon < 0) {
            throw new ParseException("Expected ':' in " + s);
        }
        
        String pointPart = s.substring(0, firstColon);
        String expressionPart = s.substring(firstColon + 1);
        int firstComma = expressionPart.indexOf(',');
        String rest;
        
        if (firstComma > 0) {
            rest = expressionPart.substring(firstComma + 1);
            expressionPart = expressionPart.substring(0, firstComma);
        } else {
            rest = null;
        }
        
        CompactSet locations = parseLocationSet(env, pointPart, 0);
        CompactSet result = parseBasicExpression(locations, env, expressionPart);
        JBCClass curClass = null;
        
        while (rest != null) {
            CompactSet newResult = new CompactSet();
            int comma = rest.indexOf(',');
            String fieldName;
        
            if (comma > 0) {
                fieldName = rest.substring(0, comma);
                rest = rest.substring(comma + 1);
            } else {
                fieldName = rest;
                rest = null;
            }
            
            if (fieldName.equals("[]")) {
                UserField f = env.getClass("java.lang.Object").registerUserField("arrayelement", false);
                
                for (Enumeration e = result.elements(); e.hasMoreElements();) {
                    JBCValuePoint p = (JBCValuePoint)e.nextElement();
                    JBCValuePoint newP = new JBCValuePoint(p.getLocation(),
                        p.getExpression().makeUserFieldExpression(f));
                    
                    newResult.add(newP);
                }
            } else {
                for (Enumeration e = result.elements(); e.hasMoreElements();) {
                    JBCValuePoint p = (JBCValuePoint)e.nextElement();
		    JBCField f = parseFieldExpression((JBCLocation)p.getLocation(), env, fieldName, curClass);
		    JBCType t = f.getFieldType();
                
		    if (f.isStatic()) {
		      throw new ParseException("Static field occurred in dereference expression: " + f);
		    }
                
		    if (t instanceof JBCObjectType) {
		      curClass = ((JBCObjectType)t).getClassDef();
		    } else {
		      curClass = null;
		    }
                
                    JBCValuePoint newP = new JBCValuePoint(p.getLocation(),
                        p.getExpression().makeNonstaticFieldExpression(f));
                    
                    newResult.add(newP);
                }
            }
                
            result = newResult;
        }
        
        return result;
    }
}
