// Static methods for manipulating the AST.

package daikon.tools.jtb;

import syntaxtree.*;
import visitor.*;
import java.lang.reflect.*;
import java.io.*;
import java.util.*;
// import util.*;
// import org.apache.regexp.*;
import JavaParser;
import ParseException;
import utilMDE.Assert;
import utilMDE.UtilMDE;
import utilMDE.ArraysMDE;

import daikon.*;

public class Ast {

  public final static String lineSep = System.getProperty("line.separator");

  ///////////////////////////////////////////////////////////////////////////
  /// Visitors
  ///

  // Reads an AST from the input stream, applies the visitor to the
  // AST, and writes the resulting AST to the output stream
  public static void applyVisitor(Reader input, Writer output,
                                  Visitor visitor) {
    JavaParser parser = new JavaParser(input);
    Node root = null;
    try {
      root = parser.CompilationUnit();
    }
    catch (ParseException e) {
      e.printStackTrace();
      System.exit(1);
    }
    root.accept(visitor);
    // This is unfortunately necessary because TreeDumper dies if line or
    // column numbers are out of sync.  Also see InsertCommentFormatter.
    root.accept(new TreeFormatter(2, 80));
    root.accept(new TreeDumper(output));
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Printing and parsing
  ///

  // Prints an AST to a String
  // This version does not blow away formatting information in the tree.
  public static String print(Node n) {
    StringWriter w = new StringWriter();
    n.accept(new SimpleTreeDumper(w));
    return removeWhitespace(w.toString());
  }

  // Prints the line enclosing a node
  public static String printCurrentLine(Node n) {
    Node current = n;
    while (current.getParent() != null &&
	   Ast.print(current.getParent()).indexOf(lineSep) < 0) {
      current = current.getParent();
    }
    return Ast.print(current);
  }


  // Creates an AST from a String
  public static Node create(String type, String stringRep) {
    JavaParser parser = new JavaParser(new StringReader(stringRep));
    Node n = null;
    try {
      Method m = JavaParser.class.getMethod(type, null);
      n = (Node) m.invoke(parser, null);
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }
    return n;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Names (fully qualified and otherwise)
  ///

  public static String getName(FormalParameter p) {
    String name = print(p.f2);
    int startBrackets = name.indexOf('[');
    if (startBrackets == -1) {
      return name;
    } else {
      return name.substring(0, startBrackets);
    }
  }

  public static String getType(FormalParameter p) {
    String type = print(p.f1);
    String name = print(p.f2);

    while (name.endsWith("[]")) {
      type += "[]";
      name = name.substring(0, name.length()-2);
    }
    return type;
  }


  public static String getName(MethodDeclaration m) {
    return m.f2.f0.tokenImage;
  }

  public static String getName(ConstructorDeclaration m) {
    return m.f1.tokenImage;
  }

  // Returns the name of the package for this compilation unit.  If no
  // package was specified, null is returned.
  public static String getPackage(CompilationUnit u) {
    NodeOptional o = u.f0;
    if (o.present()) {
      PackageDeclaration p = (PackageDeclaration) o.node;
      return Ast.print(p.f1);
    } else {
      return null;
    }
  }

  // Return the fully qualified name of the method (not including params).
  // <package>.<class>*.<method>
  // If the method is in an anonymous inner class, "$inner" is used to
  // represent the name of the inner class.
  public static String getFullName(MethodDeclaration method) {
    String className = getClassName(method);
    String methodName = getName(method);
    return className + methodName;
  }


  // Used to be called "getFullName", but that was misleading.
  // Returns the fully qualified signature of a method.
  // <package>.<class>*.<method>(<params>)
  // If the method is in an anonymous inner class, "$inner" is used to
  // represent the name of the inner class.
  public static String getFullSignature(MethodDeclaration method) {
    String className = getClassName(method);
    String methodDeclarator = getMethodDeclarator(method);
    return className + methodDeclarator;
  }

  // Return the fully qualified name of the class containing the node,
  // including the trailing period!
  // <package>.<class>*.<method>
  public static String getClassName(Node n) {
    String packageName;
    CompilationUnit unit
      = (CompilationUnit) ((n instanceof CompilationUnit) ? n
                           : Ast.getParent(CompilationUnit.class, n));
    String getPackage = getPackage(unit);
    if (getPackage != null) {
      packageName = getPackage + ".";
    } else {
      packageName = "";
    }

    String className = "";
    // Need to double-check this logic.
    if (n instanceof TypeDeclaration) {
      // use the ClassDeclaration, the InterfaceDeclaration, or the ";"
      n = ((TypeDeclaration)n).f0.choice;
    }
    if (n instanceof ClassDeclaration) {
      n = ((ClassDeclaration)n).f1; // use the UnmodifiedClassDeclaration
    }
    if (n instanceof InterfaceDeclaration) {
      n = ((InterfaceDeclaration)n).f1; // use the UnmodifiedInterfaceDeclaration
    }
    if (n instanceof UnmodifiedClassDeclaration) {
      className = ((UnmodifiedClassDeclaration)n).f1.tokenImage + ".";
    }
    if (n instanceof UnmodifiedInterfaceDeclaration) {
      className = ((UnmodifiedInterfaceDeclaration)n).f1.tokenImage + ".";
    }

    Node currentNode = n;
    while (true) {
      ClassBody b = (ClassBody) getParent(ClassBody.class, currentNode);
      if (b == null) {
        break;
      }
      Node n1 = b.getParent();
      if (n1 instanceof UnmodifiedClassDeclaration) {
        String s = ((UnmodifiedClassDeclaration) n1).f1.tokenImage;
        className = s + "." + className;
        currentNode = n1;
      } else {
        className = "$inner" + "." + className;
        currentNode = b;
      }
    }
    return packageName + className;
  }

  public static void setName(MethodDeclaration m, String name) {
    m.f2.f0.tokenImage = name;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Comments
  ///

  // Add the comment to the first regular token in the tree
  public static NodeToken addComment(Node n, String comment) {
    return addComment(n, comment, false);
  }

  // Add the comment to the first regular token in the tree.
  // If first is set, then it is inserted before all other special tokens.
  public static NodeToken addComment(Node n, String comment, boolean first) {
    NodeToken nt = new NodeToken(comment);
    addComment(n, nt, first);
    return nt;
  }

  // Add the comment to the first regular token in the tree
  public static void addComment(Node n, NodeToken comment) {
    addComment(n, comment, false);
  }

  // Add the comment to the first regular token in the tree
  // If first is set, then the comment is inserted before all other special tokens.
  public static void addComment(Node n, NodeToken comment, boolean first) {
    class AddCommentVisitor extends DepthFirstVisitor {
      private boolean seenToken = false;
      private NodeToken comment;
      private boolean first;
      public AddCommentVisitor(String comment, boolean first) {
        this.comment = new NodeToken(comment);
        this.first = first;
      }
      public AddCommentVisitor(NodeToken comment, boolean first) {
        this.comment = comment;
        this.first = first;
      }
      public void visit(NodeToken node) {
        if (! seenToken) {
          seenToken = true;
          if (first && (node.numSpecials() > 0)) {
            comment.beginLine = node.getSpecialAt(0).beginLine;
            comment.beginColumn = node.getSpecialAt(0).beginColumn;
            // System.out.println("Set from special <" + node.getSpecialAt(0) + ">");
          } else {
            comment.beginLine = node.beginLine;
            comment.beginColumn = node.beginColumn;
          }
          if (first) {
            addFirstSpecial(node, comment);
          } else {
            node.addSpecial(comment);
          }
          // System.out.println("comment (" + comment.beginLine + "," + comment.beginColumn + ") = " + comment.tokenImage + "; node (" + node.beginLine + "," + node.beginColumn + ")= " + node.tokenImage);
        }
      }
    }
    n.accept(new AddCommentVisitor(comment, first));
  }

  // Adds the comment to the first regular token in the tree, *before* all
  // other special tokens.
  public static void addFirstSpecial(NodeToken n, NodeToken s) {
    if ( n.specialTokens == null ) n.specialTokens = new Vector();
    n.specialTokens.insertElementAt(s, 0);
    s.setParent(n);
  }


  // Return the first NodeToken after (all of) the specified Node.
  public static NodeToken nodeTokenAfter(Node n) {
    // After the traversal, the "lastNodeToken" slot contains the
    // last NodeToken visited.
    class LastNodeTokenVisitor extends DepthFirstVisitor {
      public NodeToken lastNodeToken = null;
      public void visit(NodeToken node) {
        lastNodeToken = node;
      }
    }
    class NextNodeTokenVisitor extends DepthFirstVisitor {
      private boolean seenPredecessor = false;
      public NodeToken nextNodeToken;
      private NodeToken predecessor;
      public NextNodeTokenVisitor(NodeToken predecessor) {
        this.predecessor = predecessor;
      }
      public void visit(NodeToken node) {
        if (! seenPredecessor) {
          if (node == predecessor) {
            seenPredecessor = true;
          }
        } else if (nextNodeToken == null) {
          nextNodeToken = node;
        }
      }
    }

    // set predecessor
    LastNodeTokenVisitor lntv = new LastNodeTokenVisitor();
    n.accept(lntv);
    NodeToken predecessor = lntv.lastNodeToken;
    if (predecessor == null) {
      throw new Error("No lastNodeToken for " + n);
    }

    // We don't know how high in the tree we need to go in order to find a
    // successor.  This has bad worst-case performance, but should be OK in
    // practice.
    NodeToken result = null;
    Node parent = n.getParent();
    while ((result == null) && (parent != null)) {
      NextNodeTokenVisitor nntv = new NextNodeTokenVisitor(predecessor);
      parent.accept(nntv);
      result = nntv.nextNodeToken;
      parent = parent.getParent();
    }
    if (result == null) {
      throw new Error("No nextNodeToken for " + n);
    }
    return result;
  }


  // Removes all the special tokens (annotations and other comments)
  // from the first regular token in the method
  public static void removeAnnotations(MethodDeclaration m) {
    class RemoveAnnotationsVisitor extends DepthFirstVisitor {
      private boolean seenToken = false;
      public void visit(NodeToken n) {
        if (! seenToken) {
          seenToken = true;
          n.specialTokens = null;
        }
      }
    }

    m.accept(new RemoveAnnotationsVisitor());
  }


  ///////////////////////////////////////////////////////////////////////////
  /// Whitespace
  ///

 public static String removeWhitespace(String arg) {
    arg = removeWhitespaceAfter(arg, ".");
    arg = removeWhitespaceAfter(arg, "(");
    arg = removeWhitespaceBefore(arg, ")");
    return arg;
  }

  public static String removeWhitespaceAfter(String arg, String delimiter) {
    // String orig = arg;
    arg = arg.trim();
    int delim_index = arg.indexOf(delimiter);
    while (delim_index > -1) {
      int non_ws_index = delim_index+1;
      while ((non_ws_index < arg.length())
             && (Character.isWhitespace(arg.charAt(non_ws_index)))) {
        non_ws_index++;
      }
      // System.out.println("'" + arg.charAt(non_ws_index) + "' not a space character at " + non_ws_index + " in: " + arg);
      if (non_ws_index != delim_index+1) {
        arg = arg.substring(0, delim_index + 1) + arg.substring(non_ws_index);
      }
      delim_index = arg.indexOf(delimiter, delim_index+1);
    }
    return arg;
  }

  public static String removeWhitespaceBefore(String arg, String delimiter) {
    // System.out.println("removeWhitespaceBefore(\"" + arg + "\", \"" + delimiter + "\")");
    // String orig = arg;
    arg = arg.trim();
    int delim_index = arg.indexOf(delimiter);
    while (delim_index > -1) {
      int non_ws_index = delim_index-1;
      while ((non_ws_index >= 0)
             && (Character.isWhitespace(arg.charAt(non_ws_index)))) {
        non_ws_index--;
      }
      // System.out.println("'" + arg.charAt(non_ws_index) + "' not a space character at " + non_ws_index + " in: " + arg);
      if (non_ws_index != delim_index-1) {
        arg = arg.substring(0, non_ws_index + 1) + arg.substring(delim_index);
      }
      delim_index = arg.indexOf(delimiter, non_ws_index+2);
    }
    return arg;
  }


  ///////////////////////////////////////////////////////////////////////////
  /// PptMap manipulation
  ///

  // Result is a Vector of PptTopLevel elements
  public static Vector getMatches(PptMap ppts, MethodDeclaration methdecl) {
    String classname = Ast.getClassName(methdecl);
    if (classname.endsWith(".")) {
      classname = classname.substring(0, classname.length() - 1);
    }
    String methodname = Ast.getName(methdecl);
    List method_params = getParameters(methdecl);
    return getMatches(ppts, classname, methodname, method_params);
  }

  // Result is a Vector of PptTopLevel elements
  public static Vector getMatches(PptMap ppts, ConstructorDeclaration constrdecl) {
    String classname = Ast.getClassName(constrdecl);
    if (classname.endsWith(".")) {
      classname = classname.substring(0, classname.length() - 1);
    }
    // String constrname = Ast.getName(constrdecl);
    List constr_params = getParameters(constrdecl);
    return getMatches(ppts, classname, "<init>", constr_params);
  }

  // Result is a Vector of PptTopLevel elements
  public static Vector getMatches(PptMap ppts, String classname, String methodname, List method_params) {
    // System.out.println("getMatches(" + classname + ", " + methodname + ", ...)");

    Vector result = new Vector();

    String[] param_types = new String[method_params.size()];
    {
      int i = 0;
      for (Iterator itor = method_params.iterator(); itor.hasNext(); i++) {
        FormalParameter fp = (FormalParameter) itor.next();
        param_types[i] = getType(fp);
      }
    }

    // System.out.println("getMatch goal = " + classname + " " + methodname);
    for (Iterator itor = ppts.iterator() ; itor.hasNext() ; ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      PptName ppt_name = ppt.ppt_name;
      // System.out.println("getMatch considering " + ppt_name + " (" + ppt_name.getFullClassName() + "," + ppt_name.getShortMethodName() + ")");
      if (classname.equals(ppt_name.getFullClassName())
          && methodname.equals(ppt_name.getShortMethodName())) {
        // System.out.println("getMatch: class name and method name match");
        // Class name and method name match.  Now check whether args match.
        // This is complicated by the fact that JTB doesn't give us
        // fully-qualified names.
        String pptFullMethodName = ppt_name.getFullMethodName();
        int lparen = pptFullMethodName.indexOf('(');
        int rparen = pptFullMethodName.indexOf(')');
        Assert.assert(lparen > 0);
        Assert.assert(rparen > lparen);
        String ppt_args_string = UtilMDE.
	  arglistFromJvm(pptFullMethodName.substring(lparen, rparen+1));
	Assert.assert(ppt_args_string.startsWith("("), ppt_args_string);
	Assert.assert(ppt_args_string.endsWith(")"), ppt_args_string);
	ppt_args_string = ppt_args_string.substring(1, ppt_args_string.length()-1);
        String[] ppt_args = utilMDE.UtilMDE.split(ppt_args_string, ", ");
        if ((ppt_args.length == 1)
            && (ppt_args[0].equals(""))) {
          ppt_args = new String[0];
        }
        if (ppt_args.length != param_types.length) {
          // System.out.println("arg lengths mismatch: " + ppt_args.length + ", " + param_types.length);
          continue;
        }
        boolean unmatched = false;
        for (int i=0; i < ppt_args.length; i++) {
          String ppt_arg = ppt_args[i];
          String paramtype = param_types[i];
          // System.out.println("Comparing " + ppt_arg + " to " + paramtype + ":");
          if (typeMatch(ppt_arg, paramtype)) {
            // System.out.println("Match at arg position " + i + ": " + ppt_arg + " " + paramtype);
            continue;
          }
	  // Is the below test necessary since we do arglistFromJvm above?
          String ppt_arg_nonjvm = utilMDE.UtilMDE.classnameFromJvm(ppt_arg);
          if ((ppt_arg_nonjvm != null) && typeMatch(ppt_arg_nonjvm, paramtype)) {
            // System.out.println("Match at arg position " + i + ": " + ppt_arg + " " + paramtype);
            continue;
          }
          unmatched = true;
          break;
        }
        if (unmatched) {
          // System.out.println("Unmatched; continuing");
          continue;
        }
        MergeESC.debug.debug("Ast.getMatch succeeded: " + ppt.name
			     + " to " + classname + "." + methodname
			     + "(" + UtilMDE.join(param_types, ",") + ")");
	result.add(ppt);
      }
    }
    return result;
  }

  static boolean typeMatch(String goal, String abbreviated) {
    // System.out.println("Comparing " + goal + " to " + abbreviated);
    if (abbreviated.equals(goal)) {
      return true;
    }
    // If abbreviated is missing the leading package name, permit a match
    if (goal.endsWith(abbreviated)
        && (goal.charAt(goal.length() - abbreviated.length() - 1) == '.')) {
      return true;
    }
    return false;
  }

  ///////////////////////////////////////////////////////////////////////////
  /// Reflection
  ///

  static Class getClass(Node n) {
    String ast_classname = Ast.getClassName(n);
    if (ast_classname.indexOf("$inner") != -1) {
      return null;
    }
    if (ast_classname.endsWith(".")) {
      ast_classname = ast_classname.substring(0, ast_classname.length() - 1);
    }
    return getClass(ast_classname);
  }

  static Class getClass(String s) {
    try {
      Class c = Class.forName(s);
      Assert.assert(c != null);
      return c;
    } catch (ClassNotFoundException e) {
      String orig_s = s;
      // System.out.println("Lookup failed: " + s);
      // We should have found it.  Maybe there is a name mangling problem.
      while (true) {
        int dot_pos = s.lastIndexOf('.');
        if (dot_pos == -1) {
          throw new Error("Didn't find class " + orig_s);
        }
        s = s.substring(0,dot_pos) + "$" + s.substring(dot_pos+1);
        // System.out.println("Lookup trying: " + s);
        try {
          Class c = Class.forName(s);
          Assert.assert(c != null);
          return c;
        } catch (ClassNotFoundException ex) {
        }
      }
      // throw new Error("Control cannot reach here");
    }
  }

  static Method getMethod(MethodDeclaration methoddecl) {
    Class c = getClass(methoddecl);
    return getMethod(c, methoddecl);
  }

  static Method getMethod(Class c, MethodDeclaration methoddecl) {
    String ast_methodname = Ast.getName(methoddecl);
    List ast_params = getParameters(methoddecl);

    Method[] meths = c.getMethods();

    for (int i=0; i<meths.length; i++) {
      Method meth = meths[i];
      // System.out.println("getMethod(" + c.getName() + ", " + getName(methoddecl) + ") checking " + meth.getName());
      if (! meth.getName().equals(ast_methodname)) {
        continue;
      }
      Class[] params = meth.getParameterTypes();
      if (paramsMatch(params, ast_params)) {
        // System.out.println("getMatch succeeded: " + ppt.name);
        return meth;
      }
    }
    return null;
  }

  static Constructor getConstructor(ConstructorDeclaration constructordecl) {
    Class c = getClass(constructordecl);
    return getConstructor(c, constructordecl);
  }

  static Constructor getConstructor(Class c, ConstructorDeclaration constructordecl) {
    String ast_constructorname = Ast.getName(constructordecl);
    List ast_params = getParameters(constructordecl);

    Constructor[] constrs = c.getConstructors();

    for (int i=0; i<constrs.length; i++) {
      Constructor constr = constrs[i];
      if (! constr.getName().equals(ast_constructorname)) {
        continue;
      }
      Class[] params = constr.getParameterTypes();
      if (paramsMatch(params, ast_params)) {
        // System.out.println("getMatch succeeded: " + ppt.name);
        return constr;
      }
    }
    return null;
  }

  public static boolean paramsMatch(Class[] params, List ast_params) {
    if (params.length != ast_params.size()) {
      return false;
    }
    // Now check whether args match.
    boolean unmatched = false;
    int j=0;
    for (Iterator itor = ast_params.iterator(); itor.hasNext(); j++) {
      String ast_param = getType((FormalParameter) itor.next());
      String param = params[j].getName();
      // System.out.println("Comparing " + param + " to " + ast_param + ":");
      if (! typeMatch(param, ast_param)) {
        return false;
      }
    }
    return true;
  }


  // return true if m is defined in any superclass of its class
  public static boolean isOverride(MethodDeclaration methdecl) {
    if (getParent(InterfaceDeclaration.class, methdecl) != null) {
      return false;
    }
    Class c = getClass(methdecl);
    if (c == null) {
      return false;
    }
    // System.out.println("isOverride(" + getName(methdecl) + "): class=" + c.getName() + "; super=" + c.getSuperclass());
    return isOverride(c.getSuperclass(), methdecl);
  }

  // return true if methdecl is defined in c or any of its superclasses
  public static boolean isOverride(Class c, MethodDeclaration methdecl) {
    // System.out.println("isOverride(" + c.getName() + ", " + getName(methdecl) + ")");
    Method meth = getMethod(c, methdecl);
    if (meth != null) {
      // System.out.println("isOverride => true");
      return true;
    }
    Class superclass = c.getSuperclass();
    if (superclass == null) {
      return false;
    }
    return isOverride(superclass, methdecl);
  }

  // return true if methdecl is defined in any interface of its class
  public static boolean isImplementation(MethodDeclaration methdecl) {
    if (getParent(InterfaceDeclaration.class, methdecl) != null) {
      return false;
    }
    Class c = getClass(methdecl);
    if (c == null) {
      return false;
    }
    // System.out.println("isImplementation(" + getName(methdecl) + "): class=" + c.getName());
    Class[] interfaces = c.getInterfaces();
    for (int i=0; i<interfaces.length; i++) {
      if (isImplementation(interfaces[i], methdecl)) {
        return true;
      }
    }
    return false;
  }

  // return true if methdecl is defined in c or any of its interfaces
  public static boolean isImplementation(Class c, MethodDeclaration methdecl) {
    // System.out.println("isImplementation(" + c.getName() + ", " + getName(methdecl) + ")");
    Method meth = getMethod(c, methdecl);
    if (meth != null) {
      // System.out.println("isImplementation => true");
      return true;
    }
    Class[] interfaces = c.getInterfaces();
    for (int i=0; i<interfaces.length; i++) {
      if (isImplementation(interfaces[i], methdecl)) {
        return true;
      }
    }
    return false;
  }




  ///////////////////////////////////////////////////////////////////////////
  /// Etc.
  ///

  // Following the chain of parent pointers from the child, returns
  // the first node of the specified type or a subtype.  Returns null
  // if no parent of that type.
  public static Node getParent(Class type, Node child) {
    Node currentNode = child.getParent();
    while(true) {
      if (type.isInstance(currentNode)) {
        return currentNode;
      } else {
        if (currentNode == null) {
          return null;
        }
        currentNode = currentNode.getParent();
      }
    }
  }


  public static void addDeclaration(ClassBody c, ClassBodyDeclaration d) {
    c.f1.addNode(d);
  }



  public static void setAccess(MethodDeclaration m, String access) {
    NodeListOptional options = m.f0;
    for (Enumeration e = options.elements(); e.hasMoreElements(); ) {
      NodeChoice c = (NodeChoice) e.nextElement();
      NodeToken t = (NodeToken) c.choice;
      String token = t.tokenImage;
      if (token.equals("public") || token.equals("protected") ||
          token.equals("private")) {
        t.tokenImage = access;
        return;
      }
    }
    // The method did not have any modifier
    NodeToken t = new NodeToken(access);
    NodeChoice c = new NodeChoice(t);
    options.addNode(c);
  }


  // Returns a deep copy of an AST
  public static Node copy(String type, Node n) {
    String stringRep = print(n);
    return create(type, stringRep);
  }




  // returns true if, for some node in the tree, node.tokenImage.equals(s)
  public static boolean contains(Node n, String s) {
    class ContainsVisitor extends DepthFirstVisitor {
      public boolean found = false;
      private String s;
      public ContainsVisitor(String s) {
        this.s = s;
      }
      public void visit(NodeToken node) {
        found = found || s.equals(node.tokenImage);
      }
    }
    ContainsVisitor cv = new ContainsVisitor(s);
    n.accept(cv);
    return cv.found;
  }



  // Body must begin and end with a brace.
  public static void setBody(MethodDeclaration m, String body) {
    m.f4.choice = (Block) Ast.create("Block", body);
  }

  // Returns the body of a method, including the leading "{"
  public static String getBody(MethodDeclaration m) {
    return Ast.print(m.f4.choice);
  }


  public static String getReturnType(MethodDeclaration m) {
    Node n = (Node) m.f1.f0.choice;
    return Ast.print(n);
  }

  public static String getMethodDeclarator(MethodDeclaration m) {
    MethodDeclarator d = m.f2;
    return Ast.print(d);
  }


  // Returns the parameters of the method, as a list of
  // FormalParameter objects.  Returns an empty list if there are no
  // parameters.
  public static List getParameters(MethodDeclaration m) {
    class GetParametersVisitor extends DepthFirstVisitor {
      public List parameters = new ArrayList();
      public void visit(FormalParameter p) {
        parameters.add(p);
      }
    }
    GetParametersVisitor v = new GetParametersVisitor();
    MethodDeclarator d = m.f2;
    d.accept(v);
    return v.parameters;
  }

  // Returns the parameters of the constructor, as a list of
  // FormalParameter objects.  Returns an empty list if there are no
  // parameters.
  public static List getParameters(ConstructorDeclaration cd) {
    class GetParametersVisitor extends DepthFirstVisitor {
      public List parameters = new ArrayList();
      public void visit(FormalParameter p) {
        parameters.add(p);
      }
    }
    GetParametersVisitor v = new GetParametersVisitor();
    FormalParameters fp = cd.f2;
    fp.accept(v);
    return v.parameters;
  }


  public static void addImport(CompilationUnit u, ImportDeclaration i) {
    u.f1.addNode(i);
  }


  // Returns a list of Strings, the names of all the variables in the
  // expression/conditional expression/primary expression
  public static Set getVariableNames(Node expr) {

    class GetSymbolNamesVisitor extends DepthFirstVisitor {
      public Set symbolNames = new HashSet();

      public void visit(Name n) {
        Node gp = n.getParent().getParent();
        if (gp instanceof PrimaryPrefix) {
          PrimaryExpression ggp = (PrimaryExpression) gp.getParent();
          for (Enumeration e = getPrimarySuffixes(ggp);
               e.hasMoreElements(); ) {
            PrimarySuffix s = (PrimarySuffix) e.nextElement();
            if (s.f0.choice instanceof Arguments) {
              return;
            }
          }
          symbolNames.add(Ast.print(n));
        }
      }
    }

    GetSymbolNamesVisitor v = new GetSymbolNamesVisitor();
    expr.accept(v);
    return v.symbolNames;
  }

  // Returns an Enumeration of PrimarySuffix objects
  public static Enumeration getPrimarySuffixes(PrimaryExpression p) {
    return p.f1.elements();
  }

  public static String[] invariants_for(PptTopLevel ppt, PptMap ppts) {
    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    ppt.print_invariants_maybe(pw);
    String[] invs = UtilMDE.split(sw.toString(), '\n');
    Assert.assert(invs[invs.length-1].equals(""));
    if (invs.length == 1) {
      return new String[0];
    }
    if ((invs.length == 2) && (invs[0].startsWith("No samples for "))) {
      return new String[0];
    }
    // Ignore first three, and last, lines
    Assert.assert(invs[0].equals("==========================================================================="), "Not row-of-=: " + invs[0]);
    // These might differ, because return values appear in ppt.name but not in invs[1].
    // utilMDE.Assert.assert(invs[1].equals(ppt.name), "Different names: " + invs[1] + ", " + ppt.name);
    Assert.assert(invs[2].startsWith("    Variables: "));
    return ArraysMDE.subarray(invs, 3, invs.length-1-3);
  }

}
