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
  private String className; //The class name.
  private String curMethodName; //Name of current method being parsed
  private String curMethodDeclaration;
  boolean enterMethod;   //true if the current Node is a Method
                         //declaration ie. we just entered a method.

  HashMap conditions = new HashMap();     //stores the conditional expressions
                                          //as values and the methodname as
                                          //the keys
  HashMap replaceStatements = new HashMap();  //stores the method bodies
                                              //as values and the method
                                              //declaration as the keys


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
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  /**
   * Stores the field name, if it is a boolean.
   */
  public void visit(FieldDeclaration n) {
    String resultType = Ast.print(n.f1);
    if (resultType.equals("boolean")) {
      addCondition(Ast.print(n.f2.f0) + " ==  true");
    }
    super.visit(n);
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
   * f1 -> ResultType()
   * f2 -> MethodDeclarator()
   * f3 -> [ "throws" NameList() ]
   * f4 -> ( Block() | ";" )
   */

  /**
   * It is sometimes helpful to store the method bodies of one-liner
   * methods.  They are useful as 'replace' statements when the
   * condition makes a call to that function. Here we keep track of
   * the fact that we have reached a method declaration.
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
   * f2 -> ( "[" "]" )* */
  public void visit(MethodDeclarator n) {
    //This goes on the PPT_NAME line of the spinfo file.
    //eg. QueueAr.isEmpty
    curMethodName = className + "." + Ast.print(n.f0);
    addMethod (Ast.print(n), curMethodName);
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
    //This goes on the PPT_NAME line of the spinfo file.
    //eg. QueueAr.isEmpty
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

  /**
   * extracts the values for the different cases and creates splitting
   * conditions out of them
   */
  public void visit(SwitchStatement n) {
    String switchExpression = Ast.print(n.f2);
    Collection caseValues = getCaseValues(n.f5);
     //a condition for the default case. A 'not' of all the different cases.
    StringBuffer defaultString = new StringBuffer();
    for (Iterator e = caseValues.iterator(); e.hasNext(); ) {
      String switchValue = ((String) e.next()).trim();
      if (!switchValue.equals(":")) {
	if (!(defaultString.length() == 0))
	  defaultString.append(" && ");
	defaultString.append(switchExpression + " != " + switchValue);
	addCondition(switchExpression + " == " + switchValue);
      }
    }
    addCondition(defaultString.toString());
    super.visit(n);
  }

  /**
   * @return a String[] which contains the different Integer values
   * which the case expression is tested against
   */
  public Collection getCaseValues (NodeListOptional n) {
    ArrayList values = new ArrayList();
    Enumeration e = n.elements();
    while (e.hasMoreElements()) {
      //in the nodeSequence for the switch statement,
      //the first element is always the case statement.
      // ie: case <value>: expr(); break;  | default ...
      NodeSequence ns = (NodeSequence) e.nextElement();
      SwitchLabel sl = (SwitchLabel) ns.elementAt(0);
      ns = (NodeSequence) sl.f0.choice;
      values.add(Ast.print(ns.elementAt(1)));
    }
    return values;
  }

  /**
   * f0 -> "if"
   * f1 -> "("
   * f2 -> Expression()
   * f3 -> ")"
   * f4 -> Statement()
   * f5 -> [ "else" Statement() ]
   */
  /* Extract the condition in an 'if' statement */
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
  /* Extract the condition in an 'while' statement */
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
  /* Extract the condition in an 'DoStatement' statement */
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
  /* Extract the condition in an 'for' statement */
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

  /*
   * If this statement is a one-liner (the sole statement in a
   * function), then it is saved and used as a 'replace' statement in
   * the splitter info file.
   */
  public void visit(Statement n) {
    //if we just entered the function and this is a return statement,
    //then it's a one-liner. Save the statement
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

  //////// Private methods specific to ConditionExtractor ////
  /**
   * Keep track of the method we are currently in, and create an entry
   * for it, so that the conditions can be associated with the right
   * methods.
   */
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
  public void printSpinfoFile( Writer output ) throws IOException {

    if (!replaceStatements.values().isEmpty()) {
      output.write("REPLACE\n");

      Iterator bools = replaceStatements.keySet().iterator();
      while (bools.hasNext()) {
	String declaration = (String)bools.next();
	output.write(declaration + "\n");
	output.write((String) replaceStatements.get(declaration) + "\n");
      }

      output.write("\n");
    }

    Vector method_conds;
    Iterator methods = conditions.keySet().iterator();
    while (methods.hasNext()) {
      String method = (String) methods.next();
      method_conds = (Vector)conditions.get(method);
      if (method_conds.size() > 0) {
	String temp = "PPT_NAME ";
	if (packageName != null)
	  temp = temp + packageName + ".";
	output.write(temp + method + "\n");

	for (int i = 0; i < method_conds.size(); i++) {
	  output.write((String)method_conds.elementAt(i) + "\n");
	}

	output.write("\n");
      }
    }
  }
}
