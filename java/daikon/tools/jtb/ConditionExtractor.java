package daikon.tools.jtb;

import java.util.*;
import java.io.*;
import junit.framework.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.*;

// CreateSpinfo extracts the following statements from the Java source:
// For each method:
//  * extracts all expressions in conditional statements
//    ie. if, for, which, etc.
//  * if the method body is a one-line return statement, it
//    extracts it for later substitution into expressions which
//    call this function. These statements are referred to as
//    replace statements
// For each field declaration
//  * if the field is a boolean, it stores the expression
//    "<fieldname> == true" as a splitting condition.
//
//  The method printSpinfoFile prints out these expressions and
//  replace statements in splitter info file format.

class ConditionExtractor extends DepthFirstVisitor {

  private String packageName;
  private String className;
  private String curMethodName;
  private Writer output;
  private String curMethodDeclaration;
  boolean enterMethod;   //true if the current Node is a Method
                         //declaration ie. we just entered a method.

  HashMap conditions = new HashMap();     //stores the conditional expressions
                                          //as values and the methodname as
                                          //the keys
  HashMap replaceStatements = new HashMap();  //stores the method bodies
                                              //as values and the method
                                              //declaration as the keys

  public ConditionExtractor (Writer output) {
    this.output = output;
  }


  //// DepthFirstVisitor Methods overridden by ConditionExtractor //////////////
  /////

  /**
   * f0 -> "package"
   * f1 -> Name()
   * f2 -> ";"
   */
  public void visit(PackageDeclaration n) {
    packageName = Ast.print(n.f1);
    super.visit(n);
  }

  /**
   * f0 -> "class"
   * f1 -> <IDENTIFIER>
   * f2 -> [ "extends" Name() ]
   * f3 -> [ "implements" NameList() ]
   * f4 -> ClassBody()
   */
  public void visit(UnmodifiedClassDeclaration n) {
    className = Ast.print(n.f1);
    super.visit(n);
  }

  /**
   * f0 -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
  public void visit(NestedClassDeclaration n) {
    className = Ast.print(n.f1);
    super.visit(n);
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
   * f1 -> ResultType()
   * f2 -> <IDENTIFIER>
   * f3 -> "("
   */
  public void visit(MethodDeclarationLookahead n) {
    super.visit(n);  
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  public void visit(FieldDeclaration n) {
    String resultType = Ast.print(n.f1);
    if (resultType.equals("boolean")) {
      addCondition(Ast.print(n.f2.f0) + " ==  true");
    }
    super.visit(n);
  }

  /**
   * f0 -> VariableDeclaratorId()
   * f1 -> [ "=" VariableInitializer() ]
   */
  public void visit(VariableDeclarator n) {
    super.visit(n);
  }

  /**
   * f0 -> <IDENTIFIER>
   * f1 -> ( "[" "]" )*
   */
  public void visit(VariableDeclaratorId n) {
    super.visit(n);
  }

  /**
   * f0 -> ArrayInitializer()
   *       | Expression()
   */
  public void visit(VariableInitializer n) {
    super.visit(n);
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
   * f1 -> ResultType()
   * f2 -> MethodDeclarator()
   * f3 -> [ "throws" NameList() ]
   * f4 -> ( Block() | ";" )
   */

  public void visit(MethodDeclaration n) {
    //after the next non-empty statement, the variable
    //enterMethod is set to false
    enterMethod = true;
    super.visit(n);
  }

  /**
   * f0 -> <IDENTIFIER>
   * f1 -> FormalParameters()
   * f2 -> ( "[" "]" )*
   */
  public void visit(MethodDeclarator n) {
    curMethodName = className + "." + Ast.print(n.f0);
    addMethod (Ast.print(n), curMethodName);
    super.visit(n);
  }

  /**
   * f0 -> "("
   * f1 -> [ FormalParameter() ( "," FormalParameter() )* ]
   * f2 -> ")"
   */
  public void visit(FormalParameters n) {
    super.visit(n);
  }

  /**
   * f0 -> [ "final" ]
   * f1 -> Type()
   * f2 -> VariableDeclaratorId()
   */
  public void visit(FormalParameter n) {
    super.visit(n);
  }

  /**
   * f0 -> [ "public" | "protected" | "private" ]
   * f1 -> <IDENTIFIER>
   * f2 -> FormalParameters()
   * f3 -> [ "throws" NameList() ]
   * f4 -> "{"
   * f5 -> [ ExplicitConstructorInvocation() ]
   * f6 -> ( BlockStatement() )*
   * f7 -> "}"
   */
  public void visit(ConstructorDeclaration n) {
    curMethodName = className + "." + Ast.print(n.f1);
    addMethod(className + Ast.print(n), curMethodName);
    super.visit(n);
  }

  /**
   * f0 -> "switch"
   * f1 -> "("
   * f2 -> Expression()
   * f3 -> ")"
   * f4 -> "{"
   * f5 -> ( SwitchLabel() ( BlockStatement() )* )*
   * f6 -> "}"
   */
  public void visit(SwitchStatement n) {
    //need to fix the switch statement
    String switchExpression = Ast.print(n.f2);
    String[] caseValues = getCaseValues(n.f5);
    for (int i = 0; i < caseValues.length; i++) {
      String switchValue = caseValues[i].trim();
      if (!switchValue.equals(":")) {
	addCondition(switchExpression + " == " + switchValue);
      }
    }
    super.visit(n);
  }
  
  /**
   * @return a String[] which contains the different values
   * which the case expression is tested against
   */
  public String[] getCaseValues (NodeListOptional n) {
    Vector values = new Vector();
    Enumeration e = n.elements();
    while (e.hasMoreElements()) {
      //in the nodeSequence for the switch statement,
      //the first element is always the case statement.
      // ie: case <value>: expr(); break;  | default ...
      NodeSequence ns = (NodeSequence) e.nextElement();
      SwitchLabel sl = (SwitchLabel) ns.elementAt(0);
      ns = (NodeSequence) sl.f0.choice;
      values.addElement(Ast.print(ns.elementAt(1)));
    }
    return (String[]) values.toArray(new String[0]);
  }
  
  /**
   * f0 -> "if"
   * f1 -> "("
   * f2 -> Expression()
   * f3 -> ")"
   * f4 -> Statement()
   * f5 -> [ "else" Statement() ]
   */
  public void visit(IfStatement n) {
    addCondition(Ast.print(n.f2));
    super.visit(n);
  }

  /**
   * f0 -> "while"
   * f1 -> "("
   * f2 -> Expression()
   * f3 -> ")"
   * f4 -> Statement()
   */
  public void visit(WhileStatement n) {
    super.visit(n);
    addCondition(Ast.print(n.f2));
  }

  /**
   * f0 -> "do"
   * f1 -> Statement()
   * f2 -> "while"
   * f3 -> "("
   * f4 -> Expression()
   * f5 -> ")"
   * f6 -> ";"
   */
  public void visit(DoStatement n) {
    super.visit(n);
    addCondition(Ast.print(n.f4));
  }

  /**
   * f0 -> "for"
   * f1 -> "("
   * f2 -> [ ForInit() ]
   * f3 -> ";"
   * f4 -> [ Expression() ]
   * f5 -> ";"
   * f6 -> [ ForUpdate() ]
   * f7 -> ")"
   * f8 -> Statement()
   */
  public void visit(ForStatement n) {
    super.visit(n);
    addCondition(Ast.print(n.f4));
  }


  /**
   * f0 -> LabeledStatement()
   *       | Block()
   *       | EmptyStatement()
   *       | StatementExpression() ";"
   *       | SwitchStatement()
   *       | IfStatement()
   *       | WhileStatement()
   *       | DoStatement()
   *       | ForStatement()
   *       | BreakStatement()
   *       | ContinueStatement()
   *       | ReturnStatement()
   *       | ThrowStatement()
   *       | SynchronizedStatement()
   *       | TryStatement()
   */
  public void visit(Statement n) {
    if (enterMethod && (n.f0.choice instanceof ReturnStatement)) {
      ReturnStatement rs = ((ReturnStatement) n.f0.choice);
      String returnExpression = Ast.print(rs.f1);
      addReplaceStatement( returnExpression );
      addCondition(returnExpression);
      enterMethod = false;
    } else if (!(n.f0.choice instanceof EmptyStatement)) {
      enterMethod = false;
    }
    super.visit(n);
  }

  //////// Private methods specific to ConditionExtractor
  private void addMethod(String methodDeclaration, String methodname) {
    if (!conditions.containsKey(methodname)) {
      conditions.put(methodname, new Vector());
    }
    curMethodName = methodname;
    curMethodDeclaration = methodDeclaration;
  }
  
  //add the condition to the current method's list of conditions
  private void addCondition(String cond) {
    Vector conds;
    if (conditions.isEmpty()) {
      conds = new Vector();
      conds.add(cond);
      conditions.put("OBJECT", conds);
    } else if (curMethodName == null) {
      conds = (Vector) conditions.get("OBJECT");
      conds.addElement(cond);
    } else {
      conds = (Vector)conditions.get(curMethodName);
      conds.addElement(cond);
    }
  }
  
  //store the replace statement (expression) in the hashmap
  private void addReplaceStatement(String s) {
    if (curMethodDeclaration != null) {
      replaceStatements.put(curMethodDeclaration, s);
    }
  }

  //prints out the extracted conditions in spinfo file format
  public void printSpinfoFile( ){

    if (!replaceStatements.values().isEmpty()) {
      writeOut("REPLACE\n");
    }

    Iterator bools = replaceStatements.keySet().iterator();
    while (bools.hasNext()) {
      String declaration = (String)bools.next();
      writeOut(declaration + "\n");
      writeOut((String) replaceStatements.get(declaration) + "\n");
    }

    if (!replaceStatements.values().isEmpty()) {
      writeOut("\n");
    }
    
    Vector method_conds;
    Iterator methods = conditions.keySet().iterator();
    while (methods.hasNext()) {
      String method = (String) methods.next();
      method_conds = (Vector)conditions.get(method);
      if (method_conds.size() > 0) {
	String temp = "PPT_NAME ";
	if (packageName != null)
	  temp = packageName + ".";
	writeOut(temp + method + "\n");
	for (int i = 0; i < method_conds.size(); i++) {
	  writeOut((String)method_conds.elementAt(i) + "\n");
	}
	writeOut("\n");
      }
    }
  }


  private void writeOut(String s) {
    try {
      output.write(s);
    } catch (IOException ioe) {
      ioe.printStackTrace();
    }
  }

}
