package daikon.split;

import java.util.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.tools.jtb.*;
import jtb.JavaParser;
import jtb.ParseException;
import java.io.*;


/**
 * ReplaceStatement is an immutable data structure for holding
 * the information of a replace statement from a .spinfo file.
 * See "The Daikon Invariant Detector User Manual" for more information
 * on replace statements.
 */
class ReplaceStatement {

  /**
   * The method name of a replace statement. The method name
   * should not include prefixes such as "public" or "void".
   * However prefixes such as package names are allowed. For
   * example "somePackage.myMethod" is a valid method name for this
   * field.
   */
  private String methodName;

  /**
   * The method Parameters for the method of this replace statement.
   */
  private MethodParameter[] parameters;

  /**
   * The return statement of a replace statement. See constructor
   * for details of format.
   */
  private String returnStatement;

  /**
   * Creates a new instance of ReplaceStatement.
   * @param methodDeclaration the method declaration of this replace
   *  statement.  The method declaration should not include prefixes
   *  such as "public" or "void".  However, prefixes to the method name
   *  such as the package name are allowed. For example
   *  "somePrefix.myMethod(int x)" is a valid method declaration for
   *  this constructor.
   * @param returnStatement the return statement of a replace statement.
   *  The return statement should not include "return" at its beginning
   *  or ";" at its end. For example "x + 2" is a valid return statement
   *  for this field.
   * @throws IllegalArgumentException if methodDeclaration is not a valid
   *  java method declaration (with the exception of package name like
   *  prefixes.
   */
  public ReplaceStatement(String methodDeclaration, String returnStatement)
    throws ParseException {
    methodName = "";
    this.returnStatement = returnStatement;
    // Must remove any prefixes of the method name so that the java parser
    // can parse it correctly.
    int openParenIndex = methodDeclaration.indexOf('(');
    int index = methodDeclaration.lastIndexOf('.', openParenIndex);
    if (index != -1) {
      methodName = methodDeclaration.substring(0, index + 1);
      methodDeclaration = methodDeclaration.substring(index + 1);
    }
    String replaceClass = "class c { void " + methodDeclaration + "{}}";
    Reader input = new StringReader(replaceClass);
    JavaParser parser = new JavaParser(input);
    Node root = parser.CompilationUnit();
    MethodDeclarationParser visitor = new MethodDeclarationParser();
    root.accept(visitor); // sets methodName and parameters.
  }

  /**
   * Returns the name of method represented by this ReplaceStatement.
   */
  public String getName() {
    return methodName;
  }

  /**
   * Returns the return line of the method represented by this ReplaceStatement.
   */
  public String getReturnStatement() {
    return returnStatement;
  }

  /**
   * Returns the MethodParameters of the method represented by this ReplaceStatement.
   */
  public MethodParameter[] getParameters() {
    return (MethodParameter[]) parameters.clone();
  }

  /**
   * Returns a string representation of this.
   */
  public String toString() {
    StringBuffer params = new StringBuffer();
    for (int i = 0; i < parameters.length; i++) {
      params.append(parameters[i].toString());
    }
    return methodName + "(" + params + "), " + returnStatement;
  }

  /**
   * MethodDeclarationParser is jtb syntax tree visitor for extracting
   * the name and arguments from a method declaration.  For example
   * from "someMethod(int x, int y)".  "someMethod", "int x", and "int y"
   * would be extracted.
   */
  private class MethodDeclarationParser extends DepthFirstVisitor {

    /** The parameters of the method. */
    private List /*MethodParameter*/ methodParamList;

    /** True iff presently visiting the parameters of the method. */
    private boolean enteredParameters = false;

    /**
     * Creates a new instance of MethodDeclarationParser.
     */
    private MethodDeclarationParser() {
      super();
    }

    /**
     * This method should not be directly used by users of this class.
     * Sets methodName, and parameters.
     */
    public void visit(MethodDeclaration n) {
      methodName = methodName + Ast.getName(n);
      List params = new ArrayList();
      List formalParameters = Ast.getParameters(n);
      for (int i = 0; i < formalParameters.size(); i++) {
        String paramName = Ast.getName((FormalParameter) formalParameters.get(i));
        String paramType = Ast.getType((FormalParameter) formalParameters.get(i));
        MethodParameter methodParam = new MethodParameter(paramName, paramType);
        params.add(methodParam);
      }
      parameters = (MethodParameter[]) params.toArray(new MethodParameter[0]);
    }
  }
}
