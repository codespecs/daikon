/*
Copyright (c) Robert O'Callahan <roc@cs.cmu.edu> and Carnegie Mellon University
*/

package ajax.tools.benchmarks;

import ajax.*;
import java.util.*;
import ajax.util.*;
import java.io.*;
import ajax.jbc.util.*;
import ajax.jbc.*;
import ajax.jbc.typechecker.*;
import ajax.analyzer.*;
import ajax.analyzer.util.*;

class ComparablePairsDescFile {
    private String fileName;
    private Hashtable detectors = new Hashtable();
    private ComparablePairsDescFileReader owner;

    /**
       Create an analysis tool that rewrites one Daikon .decls file.

       @param owner the container that manages the actual Ajax analysis
       @param fileName the name of the .decls file
    */
    ComparablePairsDescFile(ComparablePairsDescFileReader owner, String fileName) {
	this.fileName = fileName;
	this.owner = owner;
    }

    public String getFileName() {
	return fileName;
    }

    private static boolean isReturn(int opcode) {
        switch (opcode) {
            case OpcodeConstants.OP_areturn:
            case OpcodeConstants.OP_ireturn:
            case OpcodeConstants.OP_lreturn:
            case OpcodeConstants.OP_dreturn:
            case OpcodeConstants.OP_freturn:
            case OpcodeConstants.OP_return:
                return true;
            default:
                return false;
        }
    }
    
    private static void findReachableInstructions(byte[] code, int offset, int[] singleton, boolean[] visited) {
        while (!visited[offset]) {
            visited[offset] = true;
            
            int[] successors = JBCCodeUtilities.getReachableSuccessors(code, offset, singleton);
                                    
            if (successors.length == 1) {
                offset = successors[0];
                continue;
            } else {
                for (int i = 0; i < successors.length; i++) {
                    findReachableInstructions(code, successors[i], singleton, visited);
                }
            }
        }
    }
    
    private static int findUniqueReturn(JBCMethod method, int offset) {
        byte[] code = method.getData().getCode();
        boolean[] visited = new boolean[code.length];
        int result = -1;
                            
        findReachableInstructions(code, offset, new int[1], visited);
        
        for (int i = 0; i < visited.length; i++) {
            if (visited[i] && isReturn(code[i] & 0xFF)) {
                if (result >= 0) {
                    return -1;
                } else {
                    result = i;
                }
            }
        }
        
        return result;
    }

  public static Enumeration getExpressionTypes(JBCMethod method, int offset, JBCExpression expression) {
    BytecodeTypechecker checker = new BytecodeTypechecker(method);

    checker.setEnablePreciseClasses(false);
    checker.setTrackInstanceOf(true);

    if (expression instanceof JBCLocalVarExpression) {
            checker.execute();
            return checker.getLocalVarTypes(offset,
                    ((JBCLocalVarExpression)expression).getLocalVarIndex());
    } else if (expression instanceof JBCStackElemExpression) {
            checker.execute();
            return checker.getStackElemTypes(offset,
                    ((JBCStackElemExpression)expression).getStackElemIndex());
    } else if (expression instanceof JBCStaticFieldExpression) {
            return new SingletonEnumerator(
                    ((JBCStaticFieldExpression)expression).getField().getFieldType());
    } else if (expression instanceof JBCFieldExpression) {
            return new SingletonEnumerator(
                    ((JBCFieldExpression)expression).getField().getFieldType());
    } else {
            if (Globals.debug && !(expression instanceof JBCStaticUserFieldExpression)
                && !(expression instanceof JBCUserFieldExpression)) {
                Globals.nonlocalError("Unknown expression type!");
            }

            return null;
    }
  }    

  public static JBCType getExpressionType(JBCMethod method, int offset, JBCExpression expression) {
    Enumeration e = getExpressionTypes(method, offset, expression);
    if (e == null || !e.hasMoreElements()) {
      return null;
    } else {
      JBCType t = (JBCType)e.nextElement();
      if (e.hasMoreElements()) {
	return null;
      } else {
	return t;
      }
    }
  }

    private void badDescFile(String s) {
	System.err.println("In " + fileName + ": " + s);
	System.exit(3);
    }

    private static String getArrayElementName(JBCType t) {
	if (t instanceof JBCObjectType) {
	    return "arrayelement";
	} else if (t == JBCType.FLOAT) {
	    return "floatarrayelement";
	} else if (t == JBCType.LONG) {
	    return "longarrayelement";
	} else if (t == JBCType.DOUBLE) {
	    return "doublearrayelement";
	} else {
	    return "intarrayelement";
	}
    }

    public void configure(Analyzer analyzer) {
        int lineNum = 0;
	BufferedReader r = null;
	JBCClass javaLangObject = analyzer.getWorld().getSystemClassLoader()
	    .getClass("java.lang.Object");

        try {
	    Hashtable warnedClassesNoDebugInfo = new Hashtable();

            r = new BufferedReader(new FileReader(fileName));

            String s;
            
            while ((s = r.readLine()) != null) {
                lineNum++;
                
                if (s.equals("DECLARE")) {
                    String decl = r.readLine();
                    int lastColon = decl.lastIndexOf(':');
                    String offset = decl.substring(lastColon + 1);
                    String methodName = decl.substring(0, lastColon - 2);
                    
                    lineNum++;
                    
                    int offsetIndex;
                    
                    if (offset.startsWith("EXIT")) {
                        try {
                            offsetIndex = Integer.parseInt(offset.substring(4));
                        } catch (NumberFormatException ex) {
                            badDescFile("Bad exit number: " + offset);
                            return;
                        }
                    } else if (offset.equals("ENTER")) {
                        offsetIndex = 0;
                    } else if (offset.equals("CLASS") || offset.equals("CLASS-STATIC") || offset.equals("OBJECT")) {
                        offsetIndex = 0;
                        methodName = methodName + ".<init>";
                    } else {
                        badDescFile("Unknown offset: " + offset);
                        return;
                    }
                    
                    int lastDot = methodName.lastIndexOf('.');
                    String className = methodName.substring(0, lastDot);
                    
                    JBCClass c = owner.getAppLoader().getClass(className);
                    
                    if (c == null) {
                        badDescFile("Cannot find class " + className + " to resolve method " + methodName);
                        return;
                    }
                    
                    methodName = methodName.substring(lastDot + 1);
                    methodName = methodName.replace('/', '.');
                    
                    int firstParen = methodName.indexOf('(');
                    JBCMethod method;

                    if (firstParen >= 0) {
                      String methodSignature = methodName.substring(firstParen);

		      methodName = methodName.substring(0, firstParen);

		      method = c.getMethod(methodName, methodSignature);
                    
		      if (method == null) {
                        badDescFile("Method not found: " + methodName 
					   + " (signature \"" + methodSignature
                                           + "\") in class " + className);
                        return;
		      }
		    } else {
                      method = null;
                      for (Enumeration e = c.getMethods(); e.hasMoreElements();) {
			JBCMethod m = (JBCMethod)e.nextElement();

                        if (m.getMethodName().equals(methodName)) {
                          method = m;
                          break;
			}
		      }
		    }
                    
		    if (offsetIndex > 0) {
                        int sourceLine = offsetIndex;
                        
                        offsetIndex = JBCCodeUtilities.getBytecodeOffset(method, sourceLine);
                        
                        if (offsetIndex < 0) {
                            badDescFile("Line number " + sourceLine + " not found in " + method);
                            return;
                        } else {
                            int returnOffset = findUniqueReturn(method, offsetIndex);
                            
                            if (returnOffset < 0) {
                                badDescFile("Cannot find unique return instruction from offset " + offsetIndex + " in " + method);
                                return;
                            }
                            
                            offsetIndex = returnOffset;
                        }
                    }
                    
                    CompactSet exprs = new CompactSet();
                    String[] varNames =
                        JBCCodeUtilities.getLocalVariableNames(method, offsetIndex);

		    if (warnedClassesNoDebugInfo.get(className) == null) {
                        boolean haveLocalVarInfo = false;
			for (int i = 0; i < varNames.length; i++) {
			    if (!varNames[i].startsWith("local-")) {
                                haveLocalVarInfo = true;
				break;
			    }
			}
                        if (!haveLocalVarInfo) {
                            Globals.userError("Warning: class " + className + " does not have local variable info");
			    warnedClassesNoDebugInfo.put(className, className);
                        }
		    }

                    JBCLocation location = new JBCLocation(method, offsetIndex);
                    Hashtable exprNames = new Hashtable();
                    
                    while ((s = r.readLine()) != null && !s.equals("")) {
                        String name = s;
                        String declType = r.readLine();
                        String repType = r.readLine();
                        String compatibility = r.readLine();
                        String originalName = name;

                        Globals.writeLog(null, "Processing Daikon declaration for '" + name + "'");
                        lineNum += 4;
                        if (name.indexOf('~') < 0 && !name.endsWith(".class")
                            && !name.endsWith(".toString")) {
                            int nextDot = name.indexOf('.');
			    int nextBracket = name.indexOf('[');

			    int nextPart = nextDot;
			    if (nextPart == -1 || (nextBracket != -1 && nextBracket < nextDot)) {
			      nextPart = nextBracket;
			    }
                            String baseVar = nextPart < 0 ? name : name.substring(0, nextPart);
                            
                            name = nextPart < 0 ? null : name.substring(nextPart + 1);
                            
                            JBCExpression expr;
                            
                            if (baseVar.equals("this")) {
                                expr = JBCExpression.makeLocalVarExpression(0);
                            } else if (baseVar.equals("return")) {
                                expr = JBCExpression.makeStackElemExpression(0);
                            } else {
                                expr = null;
                                
                                for (int i = 0; i < varNames.length && expr == null; i++) {
                                    if (varNames[i].equals(baseVar)) {
                                        expr = JBCExpression.makeLocalVarExpression(i);
                                    }
                                }
                                
                                while (expr == null && name != null) {
                                    int nextBaseDot = name.indexOf('.');
				    baseVar = baseVar + "." + (nextBaseDot < 0 ? name : name.substring(0, nextBaseDot));

                                    name = nextBaseDot < 0 ? null : name.substring(nextBaseDot + 1);
                                    
                                    int lastBaseDot = baseVar.lastIndexOf('.');

				    if (lastBaseDot > -1) {
				      String staticClassName = baseVar.substring(0, lastBaseDot);

				      JBCClass staticClass = owner.getAppLoader().getClass(staticClassName);
                                    
				      if (staticClass != null) {
                                        String fieldName = baseVar.substring(lastBaseDot + 1);
                                        JBCField f = staticClass.getField(fieldName);
                                        
                                        if (f != null && f.isStatic()) {
                                            expr = JBCExpression.makeStaticFieldExpression(f);
                                        }
				      }
                                    }
                                }
                                    
                                if (expr == null) {
                                    badDescFile("Variable not found: " + baseVar + " at line " + lineNum);
                                    return;
                                }
                            }
                            
                            if (name != null) {
			        JBCType varType = getExpressionType(method, offsetIndex, expr);
				if (varType == null) {
				  badDescFile("Ambiguous type for qualified name: " + name);
				  return;
				}
                    
                                while (expr != null && name != null) {
    				    nextDot = name.indexOf('.');
				    nextBracket = name.indexOf('[');

				    nextPart = nextDot;
				    if (nextPart == -1 || (nextBracket != -1 && nextBracket < nextDot)) {
				      nextPart = nextBracket;
				    }

                                    String fieldName = nextPart < 0 ? name : name.substring(0, nextPart);

				    if (!(varType instanceof JBCObjectType)) {
					badDescFile("Trying to refer to the field " + name
						    + " of non-class type " + varType);
					return;
				    }

				    JBCObjectType objType = (JBCObjectType)varType;
                                    JBCClass containingClass;
				    if (objType.equals(JBCType.OBJECT)) {
					containingClass = null; // null reference
				    } else {
					containingClass = objType.getClassDef();
					if (containingClass == null) {
					    badDescFile("Cannot load class " + objType.getClassName()
							+ " to resolve expression: " + name);
					    return;
					}
				    }
                            
                                    name = nextPart < 0 ? null : name.substring(nextPart + 1);
                                    
				    if (containingClass == null) {
				    } else if (fieldName.equals("]")) {
				      JBCType t = objType.getArrayElementType();
				     
				      if (t == null) {
					  if (containingClass.getClassName().equals("java.util.Vector")) {
					      JBCField f = containingClass.getField("elementData");
					      if (f == null) {
						  badDescFile("Cannot find the field Vector.elementData to resolve Vector elements");
						  return;
					      }
					      expr = expr.makeNonstaticFieldExpression(f);
					      t = f.getFieldType();
					      UserField uf = javaLangObject
						  .registerUserField(getArrayElementName(t), false);
					      expr = expr.makeUserFieldExpression(uf);
					      varType = t;
					  } else {
					      badDescFile(containingClass + " is not an array; cannot resolve name containing array descriptor: " + name);
					      return;
					  }
				      } else {
					  UserField f = javaLangObject
					      .registerUserField(getArrayElementName(t), false);
					  expr = expr.makeUserFieldExpression(f);
					  varType = t;
				      }
				    } else {
				      if (fieldName.equals("size")) {
					  if (containingClass.getClassName().equals("java.util.Vector")) {
					      if (containingClass.getField("elementCount") != null) {
						  fieldName = "elementCount";
					      }
					  }
				      }

				      JBCField f = containingClass.getField(fieldName);
				      if (f == null) {
                                        badDescFile("Field not found: " + fieldName + " in " + containingClass);
                                        return;
				      }
                                    
				      if (f.isStatic()) {
                                        expr = JBCExpression.makeStaticFieldExpression(f);
				      } else {
                                        expr = expr.makeNonstaticFieldExpression(f);
				      }
				      varType = f.getFieldType();
				    }
                                }
                            }
                            
			    if (expr != null) {
				exprs.add(expr);
				exprNames.put(expr, originalName);
				if (expr instanceof JBCUserFieldExpression
				    && ((JBCUserFieldExpression)expr).getField()
				    .getFieldName().endsWith("arrayelement")) {
				    JBCExpression indexExpr = ((JBCUserFieldExpression)expr).getBase()
					.makeUserFieldExpression(javaLangObject.registerUserField("arraylength", false));
				    
				    exprs.add(indexExpr);
				    exprNames.put(indexExpr, originalName + "#");
				}
			    }
                        }
                    }
                    
                    lineNum++;
                    
                    ComparablePairsDetector detector =
                        new ComparablePairsDetector(location, exprs, exprNames, decl);
                    DatumSpecifier[] specifiers = { detector };
                    ResultListener[] listeners = { detector };
                    
                    (new UnboundedSetTracker(analyzer, specifiers, listeners)).start();
                    
                    detectors.put(decl, detector);
                }
            }
        } catch (StringIndexOutOfBoundsException ex) {
            System.err.println("Internal error reading description file at line " + lineNum + ": ");
            ex.printStackTrace();
            System.exit(2);
            return;
        } catch (IOException ex) {
            System.err.println("IO Error reading description file at line " + lineNum + ": "
                + ex.getMessage());
            System.exit(2);
            return;
	} catch (RuntimeException ex) {
            System.err.println("Error reading description file at line " + lineNum);
	    throw ex;
        } finally {
	    if (r != null) {
		try {
		    r.close();
		} catch (IOException ex) {
		}
	    }
	}
    }

    public void printRewrittenFile() throws IOException {
	BufferedWriter w = new BufferedWriter(new FileWriter(fileName + ".ajax"));
	BufferedReader r = new BufferedReader(new FileReader(fileName));

	try {
	    rewriteFile(r, w);
	} finally {
	    r.close();
	    w.close();
	}
    }

    private void rewriteFile(BufferedReader r, BufferedWriter w) throws IOException {
        int lineNum = 0;
	String s;

        w.write("// Declaration file " + fileName
              + " rewritten by ComparablePairsDescFileReader\n"
              + "// " + (new Date()).toString() + "\n"
              + "VarComparability\n"
              + "implicit\n"
              + "\n");

	while ((s = r.readLine()) != null) {
          if (s.equals("VarComparability")) {
            r.readLine(); // read and discard the current compatibility
            continue;
	  }

	  lineNum++;
          w.write(s + "\n");                

	  if (s.equals("DECLARE")) {
	    String decl = r.readLine();
	    ComparablePairsDetector detector = (ComparablePairsDetector)
	      detectors.get(decl);

	    lineNum++;
            w.write(decl + "\n");
	    if (detector == null) {
	      while ((s = r.readLine()) != null && !s.equals("")) {
		String name = s;
		String declType = r.readLine();
		String repType = r.readLine();
		String compatibility = r.readLine();
                            
                w.write(name + "\n" + declType + "\n" + repType + "\n"
                    + compatibility + "\n");
		lineNum += 4;
	      }
              if (s != null) {
                  w.write(s + "\n");
	      }
	      lineNum++;
	    } else {
	      Hashtable namesToExprs = new Hashtable();
	      Hashtable exprNames = detector.getExprNames();
              Hashtable results = detector.getResults();

              Hashtable setIndices = new Hashtable();
              Hashtable resultIndices = new Hashtable();

	      for (Enumeration e = exprNames.keys(); e.hasMoreElements();) {
		JBCExpression expr = (JBCExpression)e.nextElement();

		namesToExprs.put(exprNames.get(expr), expr);

                Object result = results.get(expr);

                if (result != null) {
  		    Object index = setIndices.get(result);

    		    if (index == null) {
		        index = new Integer(setIndices.size());
			setIndices.put(result, index);

                        for (Enumeration e2 = UnboundedSetTracker.enumerateIntermediate(result); e2.hasMoreElements();) {
			    Object setExpr = e2.nextElement();

			    if (resultIndices.get(setExpr) != null) {
			        System.err.println("Nontransitivity found!");
			        System.exit(3);
			    }
			    resultIndices.put(setExpr, index);
			}
		    }
		}
	      }

	      while ((s = r.readLine()) != null && !s.equals("")) {
		String name = s;
		String declType = r.readLine();
		String repType = r.readLine();
		String compatibility = r.readLine();
                        
		lineNum += 4;
                            
		JBCExpression expr = (JBCExpression)namesToExprs.get(name);

                if (expr == null) {
                  compatibility = "-1";
		} else {
                  Object resultIndex = resultIndices.get(expr);

                  if (resultIndex != null) {
		    compatibility = resultIndex.toString();
		  } else {
		    compatibility = "-2";
		  }

		  JBCExpression indexExpr = (JBCExpression)namesToExprs.get(name + "#");

		  if (indexExpr != null) {
		      Object indexResultIndex = resultIndices.get(indexExpr);

		      if (indexResultIndex != null) {
			  compatibility += "[" + indexResultIndex.toString() + "]";
		      } else {
			  compatibility += "[-2]";
		      }
		  }
		}

                w.write(name + "\n" + declType + "\n" + repType + "\n"
                  + compatibility + "\n");
	      }

              if (s != null) {
                  w.write(s + "\n");
	      }
              lineNum++;
	    }
	  }
        }
    }

    public void printBasicReport(Writer w) throws IOException {
        for (Enumeration e = detectors.elements(); e.hasMoreElements();) {
            ComparablePairsDetector detector = (ComparablePairsDetector)e.nextElement();
            
            w.write(detector.getDetectorName() + "\n");
            
            Hashtable exprNames = detector.getExprNames();
            Hashtable results = detector.getResults();
            
            if (results.size() == 0) {
                JBCLocation loc = detector.getLocation();
                
                w.write("// Dead code at offset " + loc.getOffset() + " in " + loc.getMethod() + "\n");
            } else {
                for (Enumeration e2 = exprNames.keys(); e2.hasMoreElements();) {
                    JBCExpression expr = (JBCExpression)e2.nextElement();
                    
                    w.write((String)exprNames.get(expr) + "\n");
                    
                    StringBuffer buf = new StringBuffer();
                    
                    Object o = results.get(expr);
                    
                    if (o != null) {
                        for (Enumeration e3 = UnboundedSetTracker.enumerateIntermediate(o); e3.hasMoreElements();) {
                            if (buf.length() > 0) {
                                buf.append(" ");
                            }
                            buf.append(exprNames.get(e3.nextElement()));
                        }
                    }
                    
                    w.write(buf.toString() + "\n");
                }
            }
                        
            w.write("\n");
        }
    }
}
