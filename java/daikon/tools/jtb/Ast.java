package daikon.tools.jtb;

import daikon.*;
import daikon.SignaturesUtil;
import daikon.inv.Equality;
import daikon.inv.Invariant;
import daikon.inv.filter.*;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import jtb.JavaParser;
import jtb.ParseException;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.signature.qual.BinaryName;
import org.checkerframework.checker.signature.qual.ClassGetName;
import org.plumelib.util.StringsPlume;

/** Static methods for manipulating the AST. */
@SuppressWarnings({"rawtypes", "nullness"}) // not generics-correct
public class Ast {

  /** The line separator. */
  private static final String lineSep = System.lineSeparator();

  // ///////////////////////////////////////////////////////////////////////////
  // Visitors
  //

  // Reads an AST from the input stream, applies the visitor to the AST,
  // reformats only to insert comments, and writes the resulting AST to the
  // output stream.
  public static void applyVisitorInsertComments(
      String javafilename, Node root, Writer output, AnnotateVisitor visitor) {
    root.accept(visitor);
    root.accept(new InsertCommentFormatter(visitor.addedComments));
    PrintWriter writer = new PrintWriter(output, true);
    for (int i = 0; i < visitor.javaFileLines.size(); i++) {
      writer.println(visitor.javaFileLines.get(i));
    }
    writer.close();

    // root.accept(new TreeDumper(output));
  }

  // Reads an AST from the input stream, applies the visitor to the AST,
  // completely reformats the Ast (losing previous formating), and writes
  // the resulting AST to the output stream.
  public static void applyVisitorReformat(Reader input, Writer output, Visitor visitor) {
    JavaParser parser = new JavaParser(input);
    Node root;
    try {
      root = parser.CompilationUnit();
    } catch (ParseException e) {
      e.printStackTrace();
      throw new Daikon.UserError("ParseException in applyVisitorReformat");
    }
    root.accept(visitor);
    // This is unfortunately necessary because TreeDumper dies if line or
    // column numbers are out of sync.  Also see InsertCommentFormatter and
    // applyVisitorInsertComments.
    root.accept(new TreeFormatter(2, 80));
    root.accept(new TreeDumper(output));
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Printing and parsing
  //

  // Formats an AST as a String.
  // This version does not reformat the tree (which blows away formatting
  // information).  The call to "removeWhitespace" may do the wrong thing
  // for embedded strings, however.  In any event, the result is not
  // intended for direct human consumption.
  public static String format(Node n) {
    StringWriter w = new StringWriter();
    n.accept(new TreeDumper(w));
    // This is incorrect. A "//" comment ending in a period, for example, will
    // cause the line after it to become part of the comment as well.
    // If not intended for human consumption, why remove whitespace?
    // return removeWhitespace(w.toString());
    return removeWhitespace(quickFixForInternalComment(w.toString()));
  }

  // Formats an AST as a String.
  // This is a debugging version that outputs all tree nodes:
  // terminals and non-terminals alike.
  public static String formatEntireTree(Node n) {
    StringWriter w = new StringWriter();
    n.accept(new TreeDumper(w, true));
    // delete empty lines, but otherwise leave text alone
    return w.toString().replaceAll("(?m)^[ \t]*\r?\n", "");
  }

  /**
   * This method translates a line like
   *
   * <pre>{@code
   * a statement; // a comment
   * }</pre>
   *
   * into
   *
   * <pre>
   * a statement; // a comment//
   * </pre>
   *
   * @param s a Java code line that might contain a comment
   * @return the line with its comment, if any, tweaked
   */
  public static String quickFixForInternalComment(String s) {
    StringBuilder b = new StringBuilder();
    String[] split = StringsPlume.splitLines(s);
    for (int i = 0; i < split.length; i++) {
      String line = split[i];
      b.append(line);
      if (line.indexOf("//") != -1) {
        b.append("//");
        b.append(lineSep);
        b.append("/* */");
      }
      b.append(lineSep);
    }
    return b.toString();
  }

  // Formats the line enclosing a node
  public static String formatCurrentLine(Node n) {
    Node current = n;
    while (current.getParent() != null && format(current.getParent()).indexOf(lineSep) < 0) {
      current = current.getParent();
    }
    return format(current);
  }

  /**
   * Creates an AST from a String.
   *
   * @param type the type of the result
   * @param stringRep the string to parse
   * @return an AST created from the string
   */
  public static Node create(String type, String stringRep) {
    return create(type, new Class<?>[] {}, new Object[] {}, stringRep);
  }

  // Creates an AST from a String
  public static Node create(String type, Class<?>[] argTypes, Object[] args, String stringRep) {
    JavaParser parser = new JavaParser(new StringReader(stringRep));
    Node n;
    try {
      Method m = JavaParser.class.getMethod(type, argTypes);
      n = (Node) m.invoke(parser, args);
    } catch (Exception e) {
      System.err.println("create(" + type + ", \"" + stringRep + "\")");
      e.printStackTrace();
      throw new Daikon.UserError("Error in Ast.create");
    }
    return n;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Names (fully qualified and otherwise)
  //

  /**
   * Returns true if the string is an access modifier: "public, "protected", or "private".
   *
   * @param s a string
   * @return true if the string is an access modifier
   */
  public static boolean isAccessModifier(String s) {
    return s.equals("public") || s.equals("protected") || s.equals("private");
  }

  // f4 -> VariableDeclaratorId()
  public static String getName(FormalParameter p) {
    String name = format(p.f4);
    int startBrackets = name.indexOf('[');
    if (startBrackets == -1) {
      return name;
    } else {
      return name.substring(0, startBrackets);
    }
  }

  // f2 -> Type()
  // f4 -> VariableDeclaratorId()
  public static String getType(FormalParameter fp) {

    StringWriter w = new StringWriter();
    fp.accept(new TreeDumper(w));

    FormalParameter p = (FormalParameter) create("FormalParameter", w.toString());

    p.accept(new TreeFormatter());

    String type = format(p.f2);
    String name = format(p.f4);

    // format() removes whitespace around brackets, so this test is safe.
    while (name.endsWith("[]")) {
      type += "[]";
      name = name.substring(0, name.length() - 2);
    }

    return type;
  }

  // f2 -> MethodDeclarator()
  public static String getName(MethodDeclaration m) {
    return m.f2.f0.tokenImage;
  }

  // f1 -> <IDENTIFIER>
  public static String getName(ConstructorDeclaration m) {
    return m.f1.tokenImage;
  }

  // Returns the name of the package for this compilation unit, or null if
  // no package was specified.
  public static @Nullable String getPackage(CompilationUnit u) {
    NodeOptional o = u.f0;
    if (o.present()) {
      PackageDeclaration p = (PackageDeclaration) o.node;
      return format(p.f2); // f2 -> Name()
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
    return className + "." + methodName;
  }

  // Used to be called "getFullName", but that was misleading.
  // Returns the fully qualified signature of a method.
  // <package>.<class>*.<method>(<params>)
  // If the method is in an anonymous inner class, "$inner" is used to
  // represent the name of the inner class.
  public static String getFullSignature(MethodDeclaration method) {
    String className = getClassName(method);
    String methodDeclarator = getMethodDeclarator(method);
    return className + "." + methodDeclarator;
  }

  /**
   * Returns the classname if the given type declaration declares a ClassOrInterfaceDeclaration.
   * Otherwise returns null.
   */
  public static @Nullable @BinaryName String getClassNameForType(TypeDeclaration d) {

    // Grammar production for TypeDeclaration:
    // f0 -> ";"
    //       | Modifiers() ( ClassOrInterfaceDeclaration(modifiers) | EnumDeclaration(modifiers) |
    // AnnotationTypeDeclaration(modifiers) )

    NodeChoice c = d.f0;
    if (c.which == 0) {
      return null;
    } else {
      NodeSequence seq = (NodeSequence) c.choice;
      NodeChoice c2 = (NodeChoice) seq.elementAt(1);
      if (c2.choice instanceof ClassOrInterfaceDeclaration) {
        return getClassName(c2.choice);
      } else {
        return null;
      }
    }
  }

  /** Return the fully qualified name of the class containing the node. */
  public static @BinaryName String getClassName(Node d) {

    ClassOrInterfaceDeclaration n =
        (d instanceof ClassOrInterfaceDeclaration)
            ? (ClassOrInterfaceDeclaration) d
            : (ClassOrInterfaceDeclaration) Ast.getParent(ClassOrInterfaceDeclaration.class, d);

    String packageName;
    CompilationUnit unit = (CompilationUnit) getParent(CompilationUnit.class, n);
    String getPackage = getPackage(unit);
    if (getPackage != null) {
      packageName = getPackage + ".";
    } else {
      packageName = "";
    }

    String className = n.f1.tokenImage + "."; // f1 -> <IDENTIFIER>

    Node currentNode = n;
    while (true) {
      ClassOrInterfaceBody b =
          (ClassOrInterfaceBody) getParent(ClassOrInterfaceBody.class, currentNode);
      if (b == null) {
        break;
      }
      Node n1 = b.getParent();
      assert n1 instanceof ClassOrInterfaceDeclaration;
      if (isInner((ClassOrInterfaceDeclaration) n1)) {
        // TODO: This works for anonymous classes (maybe), but is wrong for
        // non-anonymous inner classes.
        className = "$inner." + className;
        currentNode = b;
      } else {
        String s = ((ClassOrInterfaceDeclaration) n1).f1.tokenImage;
        className = s + "." + className;
        currentNode = n1;
      }
    }

    String result = packageName + className;
    if (result.endsWith(".")) {
      result = result.substring(0, result.length() - 1);
    }

    @SuppressWarnings("signature") // string concatenation, etc.
    @BinaryName String result_bnfna = result;
    return result_bnfna;
  }

  // f2 -> MethodDeclarator()
  public static void setName(MethodDeclaration m, String name) {
    m.f2.f0.tokenImage = name;
  }

  // f1 -> [ AssignmentOperator() Expression() ]
  // Return the primary expression on the left-hand side of an assignment
  public static PrimaryExpression assignment2primaryexpression(Expression n) {
    // All this could perhaps be replaced with an ad-hoc visitor, as is
    // done in nodeTokenAfter().  But it is written now, so leave it as is.

    assert n.f1.present();
    ConditionalExpression ce = n.f0;
    assert !ce.f1.present();
    ConditionalOrExpression coe = ce.f0;
    assert !coe.f1.present();
    ConditionalAndExpression cae = coe.f0;
    assert !cae.f1.present();
    InclusiveOrExpression ioe = cae.f0;
    assert !ioe.f1.present();
    ExclusiveOrExpression eoe = ioe.f0;
    assert !eoe.f1.present();
    AndExpression ande = eoe.f0;
    assert !ande.f1.present();
    EqualityExpression ee = ande.f0;
    assert !ee.f1.present();
    InstanceOfExpression iofe = ee.f0;
    assert !iofe.f1.present();
    RelationalExpression re = iofe.f0;
    assert !re.f1.present();
    ShiftExpression se = re.f0;
    assert !se.f1.present();
    AdditiveExpression adde = se.f0;
    assert !adde.f1.present();
    MultiplicativeExpression me = adde.f0;
    assert !me.f1.present();
    UnaryExpression ue = me.f0;
    UnaryExpressionNotPlusMinus uenpm = (UnaryExpressionNotPlusMinus) ue.f0.choice;
    PostfixExpression pfe = (PostfixExpression) uenpm.f0.choice;
    assert !pfe.f1.present();
    PrimaryExpression pe = pfe.f0;
    return pe;
  }

  public static String fieldName(PrimaryExpression pe) {

    // System.out.println("fieldName(" + pe + ")");

    // PrimaryExpression:
    // f0 -> PrimaryPrefix()
    // f1 -> ( PrimarySuffix() )*

    // First, try to get a name from the PrimarySuffix.

    NodeListOptional pslist = pe.f1;
    if (pslist.size() > 0) {
      PrimarySuffix ps = (PrimarySuffix) pslist.elementAt(pslist.size() - 1);
      NodeChoice psnc = ps.f0;
      // PrimarySuffix:
      // f0 -> "." "super"
      //       | "." "this"
      //       | "." AllocationExpression()
      //       | MemberSelector()
      //       | "[" Expression() "]"
      //       | "." <IDENTIFIER>
      //       | Arguments()
      switch (psnc.which) {
        case 5:
          NodeSequence sn = (NodeSequence) psnc.choice;
          assert sn.size() == 2;
          return ((NodeToken) sn.elementAt(1)).tokenImage;
      }
    }

    // If it was impossible to get a name from the PrimarySuffix,
    // try the PrimaryPrefix.

    // PrimaryPrefix:
    // f0 -> Literal()
    //       | ( <IDENTIFIER> "." )* "this"
    //       | "super" "." <IDENTIFIER>
    //       | ClassOrInterfaceType() "." "super" "." <IDENTIFIER>
    //       | "(" Expression() ")"
    //       | AllocationExpression()
    //       | ResultType() "." "class"
    //       | Name()
    PrimaryPrefix pp = pe.f0;
    NodeChoice ppnc = pp.f0;
    switch (ppnc.which) {
      case 2:
        NodeSequence sn = (NodeSequence) ppnc.choice;
        assert sn.size() == 3;
        return ((NodeToken) sn.elementAt(2)).tokenImage;
      case 7:
        return fieldName((Name) ppnc.choice);
    }

    return null;
  }

  public static String fieldName(Name n) {
    NodeListOptional nlo = n.f1;
    if (nlo.present()) {
      NodeSequence ns = (NodeSequence) nlo.elementAt(nlo.size() - 1);
      assert ns.size() == 2;
      NodeToken nt = (NodeToken) ns.elementAt(1);
      return nt.tokenImage;
    } else {
      return n.f0.tokenImage;
    }
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Comments
  //

  // Add the comment to the first regular token in the tree, after all
  // other special tokens (comments) associated with that token.
  public static NodeToken addComment(Node n, String comment) {
    return addComment(n, comment, false);
  }

  // Add the comment to the first regular token in the tree.
  // If first is true, then it is inserted before all other special tokens;
  // otherwise, it is inserted after them.
  public static NodeToken addComment(Node n, String comment, boolean first) {
    NodeToken nt = new NodeToken(comment);
    addComment(n, nt, first);
    return nt;
  }

  // Add the comment to the first regular token in the tree, after all
  // other special tokens (comments) associated with that token.
  public static void addComment(Node n, NodeToken comment) {
    addComment(n, comment, false);
  }

  /**
   * Add the comment to the first regular token in the tree, as a "special token" (comment).
   *
   * <p>If first is true, then it is inserted before all other special tokens; otherwise, it is
   * inserted after them.
   *
   * <p>Postcondition (make sure you preserve it if you modify this method): comment.beginLine and
   * comment.beginColumn have been assigned the line and column number where this comment will go.
   */
  public static void addComment(Node n, NodeToken comment, boolean first) {
    class AddCommentVisitor extends DepthFirstVisitor {
      private boolean seenToken = false;
      private final NodeToken comment;
      private final boolean first;

      public AddCommentVisitor(NodeToken comment, boolean first) {
        this.comment = comment;
        this.first = first;
      }

      @Override
      public void visit(NodeToken node) {
        if (!seenToken) {
          seenToken = true;
          if (first && (node.numSpecials() > 0)) {
            comment.beginLine = node.getSpecialAt(0).beginLine;
            comment.beginColumn = node.getSpecialAt(0).beginColumn;
            // System.out.println("Set from special <<<" + node.getSpecialAt(0).trim() + ">>>");
          } else {
            comment.beginLine = node.beginLine;
            comment.beginColumn = node.beginColumn;
          }
          if (first) {
            addFirstSpecial(node, comment);
          } else {
            node.addSpecial(comment);
          }
          // System.out.println("comment (" + comment.beginLine + "," + comment.beginColumn + ") = "
          // + comment.tokenImage + "; node (" + node.beginLine + "," + node.beginColumn + ")= " +
          // node.tokenImage);
        }
      }
    }

    // String briefComment = comment.toString();
    // if (briefComment.length() > 70)
    //     briefComment = briefComment.substring(0,70) + "...";
    // System.out.printf("addComment at %d:%d, first=%b%n  <<<%s>>>%n",
    //                    comment.beginLine, comment.beginColumn, first, briefComment);

    n.accept(new AddCommentVisitor(comment, first));
  }

  // This seems to set the beginLine and beginColumn for the comment, but
  // how is the comment inserted into the tree?  The lines that ought to do
  // that are commented out (without explanation).  The explanation is that
  // despite the comment, this does NOT do insertion.  It just determines
  // where the insertion ought to occur.  The change forces the client to
  // do the insertion.  This should be documented/fixed.  -MDE 7/13/2003

  /**
   * Add the comment to the first regular token in the tree. If first is true, then it is inserted
   * before all other special tokens; otherwise, it is inserted after them.
   *
   * <p>Exception: If first is true, may heuristically place the comment it in the middle of the
   * list of special tokens (comments), in order to place it at the same column as the real node.
   *
   * <p>Postcondition (make sure you preserve it if you modify this method): comment.beginLine and
   * comment.beginColumn have been assigned the line and column number where this comment will go.
   */
  public static void findLineAndCol(Node n, NodeToken comment, boolean first) {
    class AddCommentVisitor extends DepthFirstVisitor {
      private boolean seenToken = false;
      private final NodeToken comment;
      private final boolean first;

      public AddCommentVisitor(NodeToken comment, boolean first) {
        this.comment = comment;
        this.first = first;
      }

      @Override
      public void visit(NodeToken node) {
        if (!seenToken) {
          seenToken = true;
          if (first
              && (node.numSpecials() > 1)
              && (node.getSpecialAt(0).beginColumn
                  != node.getSpecialAt(node.numSpecials() - 1).beginColumn)) {
            // There are multiple comments, and the first and last ones
            // start at different columns.  Search from the end, finding
            // the first one with a different column.
            int i = node.numSpecials() - 1;
            while (node.getSpecialAt(i - 1).beginColumn == node.getSpecialAt(i).beginColumn) {
              i--;
            }
            comment.beginLine = node.getSpecialAt(i).beginLine;
            comment.beginColumn = node.getSpecialAt(i).beginColumn;
            // addNthSpecial(node, comment, i);
          } else if (first && (node.numSpecials() > 0)) {
            comment.beginLine = node.getSpecialAt(0).beginLine;
            comment.beginColumn = node.getSpecialAt(0).beginColumn;
            // System.out.println("findLineAndCol: set from special <<<" + node.getSpecialAt(0) +
            // ">>>");
            // addFirstSpecial(node, comment);
          } else {
            comment.beginLine = node.beginLine;
            comment.beginColumn = node.beginColumn;
            // node.addSpecial(comment);
          }
          // System.out.printf("comment placed at (%d,%d) = <<<%s>>> for node (%d,%d)= <<<%s>>>%n",
          //                   comment.beginLine, comment.beginColumn, comment.tokenImage.trim(),
          //                   node.beginLine, node.beginColumn, node.tokenImage.trim());
        }
      }
    }
    n.accept(new AddCommentVisitor(comment, first));
  }

  /** Adds the comment to the first regular token in the tree, before the ith special token. */
  @SuppressWarnings("JdkObsolete") // JTB uses Vector
  public static void addNthSpecial(NodeToken n, NodeToken s, int i) {
    if (n.specialTokens == null) {
      n.specialTokens = new Vector<NodeToken>();
    }
    n.specialTokens.insertElementAt(s, i);
    s.setParent(n);
  }

  /** Adds the comment to the first regular token in the tree, *before* all other special tokens. */
  public static void addFirstSpecial(NodeToken n, NodeToken s) {
    addNthSpecial(n, s, 0);
  }

  // Return the first NodeToken after (all of) the specified Node.
  public static NodeToken nodeTokenAfter(Node n) {
    // After the traversal, the "lastNodeToken" slot contains the
    // last NodeToken visited.
    class LastNodeTokenVisitor extends DepthFirstVisitor {
      public NodeToken lastNodeToken = null;

      @Override
      public void visit(NodeToken node) {
        lastNodeToken = node;
      }
    }
    // After the traversal, the "nextNodeToken" slot contains the token
    // visited immediately after "predecessor".  ("predecessor" should be a
    // descendant of the token from whcih traversal starts.)
    class NextNodeTokenVisitor extends DepthFirstVisitor {
      private boolean seenPredecessor = false;
      public NodeToken nextNodeToken;
      private final NodeToken predecessor;

      public NextNodeTokenVisitor(NodeToken predecessor) {
        this.predecessor = predecessor;
      }

      @Override
      @SuppressWarnings("interning")
      public void visit(NodeToken node) {
        if (!seenPredecessor) {
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
    // successor, so iteratively go higher until success.  This has bad
    // worst-case performance, but should be acceptable in practice.
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

      @Override
      public void visit(NodeToken n) {
        if (!seenToken) {
          seenToken = true;
          n.specialTokens = null;
        }
      }
    }

    m.accept(new RemoveAnnotationsVisitor());
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Whitespace
  //

  /**
   * Removes whitespace around punctuation: ., (, ), [, ].
   *
   * @param arg a string
   * @return the string with whitespace removed
   */
  public static String removeWhitespace(String arg) {
    arg = arg.trim();
    arg = StringsPlume.removeWhitespaceAround(arg, ".");
    arg = StringsPlume.removeWhitespaceAround(arg, "(");
    arg = StringsPlume.removeWhitespaceAround(arg, ")");
    arg = StringsPlume.removeWhitespaceAround(arg, "[");
    arg = StringsPlume.removeWhitespaceBefore(arg, "]");
    return arg;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // PptMap manipulation
  //

  // ///////////////////////////////////////////////////////////////////////////
  // Reflection
  //

  /**
   * Returns the class corresponding to the given node
   *
   * @param n a node
   * @return the class corresponding to the given node
   */
  public static Class<?> getClass(Node n) {
    String ast_classname = getClassName(n);
    if (ast_classname.indexOf("$inner") != -1) {
      return null;
    }
    return getClass(ast_classname);
  }

  public static Class<?> getClass(@ClassGetName String s) {
    try {
      Class<?> c = Class.forName(s);
      assert c != null;
      return c;
    } catch (ClassNotFoundException e) {
      String orig_s = s;
      // System.out.println("Lookup failed: " + s);
      // We should have found it.  Maybe there is a name mangling problem.
      // Systematically change each "." to "$" in an attempt to fix it.
      while (true) {
        int dot_pos = s.lastIndexOf('.');
        if (dot_pos == -1) {
          throw new Error("Didn't find class " + orig_s);
        }
        @SuppressWarnings("signature") // string concatenation
        @ClassGetName String new_s = s.substring(0, dot_pos) + "$" + s.substring(dot_pos + 1);
        s = new_s;
        // System.out.println("Lookup trying: " + s);
        try {
          Class<?> c = Class.forName(s);
          assert c != null;
          return c;
        } catch (ClassNotFoundException ex) {
          throw new Error("This can't happen");
        }
      }
      // throw new Error("Control cannot reach here");
    }
  }

  public static Method getMethod(MethodDeclaration methoddecl) {
    Class<?> c = getClass(methoddecl);
    return getMethod(c, methoddecl);
  }

  public static Method getMethod(Class<?> c, MethodDeclaration methoddecl) {
    String ast_methodname = getName(methoddecl);
    List<FormalParameter> ast_params = getParameters(methoddecl);

    List<Method> publicMethods = Arrays.<Method>asList(c.getMethods());
    List<Method> declaredMethods = Arrays.<Method>asList(c.getDeclaredMethods());
    List<Method> allMethods = new ArrayList<>();
    allMethods.addAll(publicMethods);
    allMethods.addAll(declaredMethods);

    Method[] meths = allMethods.toArray(new Method[0]);

    for (int i = 0; i < meths.length; i++) {

      Method meth = meths[i];
      // System.out.println("getMethod(" + c.getName() + ", " + getName(methoddecl) + ") checking "
      // + meth.getName());
      if (!typeMatch(meth.getName(), ast_methodname)) {
        continue;
      }

      Class<?>[] params = meth.getParameterTypes();
      if (paramsMatch(params, ast_params)) {
        // System.out.println("getMatch succeeded: " + ppt.name());
        return meth;
      }
    }
    return null;
  }

  public static Constructor<?> getConstructor(ConstructorDeclaration constructordecl) {
    Class<?> c = getClass(constructordecl);
    return getConstructor(c, constructordecl);
  }

  public static Constructor<?> getConstructor(Class<?> c, ConstructorDeclaration constructordecl) {
    String ast_constructorname = getName(constructordecl);

    List<FormalParameter> ast_params = getParameters(constructordecl);

    List<Constructor<?>> publicConstructors = Arrays.<Constructor<?>>asList(c.getConstructors());
    List<Constructor<?>> declaredConstructors =
        Arrays.<Constructor<?>>asList(c.getDeclaredConstructors());
    List<Constructor<?>> allConstructors = new ArrayList<>();
    allConstructors.addAll(publicConstructors);
    allConstructors.addAll(declaredConstructors);

    Constructor<?>[] constrs = allConstructors.toArray(new Constructor<?>[0]);

    for (int i = 0; i < constrs.length; i++) {

      Constructor<?> constr = constrs[i];
      if (!typeMatch(constr.getName(), ast_constructorname)) {
        continue;
      }
      Class<?>[] params = constr.getParameterTypes();
      if (paramsMatch(params, ast_params)) {
        // System.out.println("getMatch succeeded: " + ppt.name());
        return constr;
      }
    }
    return null;
  }

  public static boolean paramsMatch(Class<?>[] params, List<FormalParameter> ast_params) {

    if (params.length != ast_params.size()) {
      return false;
    }
    // Now check whether args match.
    int j = 0;
    for (Iterator<FormalParameter> itor = ast_params.iterator(); itor.hasNext(); j++) {
      String ast_param = getType(itor.next());
      Class<?> param = params[j];
      // System.out.println("Comparing " + param + " to " + ast_param + ":");
      if (!typeMatch(classnameForSourceOutput(param), ast_param)) {
        return false;
      }
    }
    return true;
  }

  // return true if m is defined in any superclass of its class
  public static boolean isOverride(MethodDeclaration methdecl) {
    ClassOrInterfaceDeclaration classOrInterface =
        (ClassOrInterfaceDeclaration) getParent(ClassOrInterfaceDeclaration.class, methdecl);
    if (classOrInterface != null && isInterface(classOrInterface)) {
      return false;
    }
    Class<?> c = getClass(methdecl);
    if (c == null) {
      return false;
    }
    // System.out.println("isOverride(" + getName(methdecl) + "): class=" + c.getName() + "; super="
    // + c.getSuperclass());
    return isOverride(c.getSuperclass(), methdecl);
  }

  // return true if methdecl is defined in c or any of its superclasses
  public static boolean isOverride(Class<?> c, MethodDeclaration methdecl) {
    // System.out.println("isOverride(" + c.getName() + ", " + getName(methdecl) + ")");
    Method meth = getMethod(c, methdecl);
    if (meth != null) {
      // System.out.println("isOverride => true");
      return true;
    }
    Class<?> superclass = c.getSuperclass();
    if (superclass == null) {
      return false;
    }
    return isOverride(superclass, methdecl);
  }

  // return true if methdecl is defined in any interface of its class
  public static boolean isImplementation(MethodDeclaration methdecl) {

    ClassOrInterfaceDeclaration classOrInterface =
        (ClassOrInterfaceDeclaration) getParent(ClassOrInterfaceDeclaration.class, methdecl);
    if (classOrInterface != null && isInterface(classOrInterface)) {
      return false;
    }
    Class<?> c = getClass(methdecl);
    if (c == null) {
      return false;
    }
    // System.out.println("isImplementation(" + getName(methdecl) + "): class=" + c.getName());
    Class<?>[] interfaces = c.getInterfaces();
    for (int i = 0; i < interfaces.length; i++) {
      if (isImplementation(interfaces[i], methdecl)) {
        return true;
      }
    }
    return false;
  }

  // return true if methdecl is defined in c or any of its interfaces
  public static boolean isImplementation(Class<?> c, MethodDeclaration methdecl) {
    // System.out.println("isImplementation(" + c.getName() + ", " + getName(methdecl) + ")");
    Method meth = getMethod(c, methdecl);
    if (meth != null) {
      // System.out.println("isImplementation => true");
      return true;
    }
    Class<?>[] interfaces = c.getInterfaces();
    for (int i = 0; i < interfaces.length; i++) {
      if (isImplementation(interfaces[i], methdecl)) {
        return true;
      }
    }
    return false;
  }

  // ///////////////////////////////////////////////////////////////////////////
  // Etc.
  //

  // Following the chain of parent pointers from the child, returns
  // the first node of the specified type or a subtype.  Returns null
  // if no parent of that type.
  public static @Nullable Node getParent(Class<?> type, Node child) {
    Node currentNode = child.getParent();
    while (true) {
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

  public static void addDeclaration(ClassOrInterfaceBody c, ClassOrInterfaceBodyDeclaration d) {
    c.f1.addNode(d);
  }

  // The "access" argument should be one of "public", "protected", or "private".
  @SuppressWarnings("JdkObsolete") // JTB uses Enumeration
  public static void setAccess(MethodDeclaration m, String access) {
    // The following four confusing lines are a following of the
    // syntax tree to get to the modifiers.
    ClassOrInterfaceBodyDeclaration decl =
        (ClassOrInterfaceBodyDeclaration) Ast.getParent(ClassOrInterfaceBodyDeclaration.class, m);
    NodeChoice nc = decl.f0;
    NodeSequence sequence = (NodeSequence) nc.choice;
    Modifiers modifiers = (Modifiers) sequence.elementAt(0);
    NodeListOptional options = modifiers.f0;
    for (Enumeration e = options.elements(); e.hasMoreElements(); ) { // non-generic due to JTB
      NodeChoice c = (NodeChoice) e.nextElement();

      if (c.choice instanceof NodeToken) {
        NodeToken t = (NodeToken) c.choice;
        String token = t.tokenImage;
        if (token.equals("public") || token.equals("protected") || token.equals("private")) {
          t.tokenImage = access;
          return;
        }
      }
    }
    // The method did not have any modifier
    NodeToken t = new NodeToken(access);
    NodeChoice c = new NodeChoice(t);
    options.addNode(c);
  }

  @SuppressWarnings("JdkObsolete") // JTB uses Enumeration
  public static void removeMethodDeclAnnotations(MethodDeclaration method) {
    // The following four confusing lines are a following of the
    // syntax tree to get to the modifiers.
    ClassOrInterfaceBodyDeclaration decl =
        (ClassOrInterfaceBodyDeclaration)
            Ast.getParent(ClassOrInterfaceBodyDeclaration.class, method);
    NodeChoice nc = decl.f0;
    NodeSequence sequence = (NodeSequence) nc.choice;
    Modifiers modifiers = (Modifiers) sequence.elementAt(0);
    NodeListOptional options = modifiers.f0;

    NodeListOptional filteredOptions = new NodeListOptional();

    for (Enumeration e = options.elements(); e.hasMoreElements(); ) { // non-generic due to JTB
      NodeChoice c = (NodeChoice) e.nextElement();

      if (!(c.choice instanceof jtb.syntaxtree.Annotation)) {
        filteredOptions.addNode(c);
      }
    }

    modifiers.f0 = filteredOptions;
  }

  // returns true if, for some node in the tree, node.tokenImage.equals(s)
  public static boolean contains(Node n, String s) {
    class ContainsVisitor extends DepthFirstVisitor {
      public boolean found = false;
      private final String s;

      public ContainsVisitor(String s) {
        this.s = s;
      }

      @Override
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

  // Returns the body of a method, including the leading "{" and trailing "}"
  public static String getBody(MethodDeclaration m) {
    return format(m.f4.choice);
  }

  public static String getReturnType(MethodDeclaration m) {
    Node n = m.f1.f0.choice;
    return format(n);
  }

  public static String getMethodDeclarator(MethodDeclaration m) {
    MethodDeclarator d = m.f2;
    return format(d);
  }

  // Returns the parameters of the method, as a list of
  // FormalParameter objects.  Returns an empty list if there are no
  // parameters.
  public static List<FormalParameter> getParameters(MethodDeclaration m) {
    class GetParametersVisitor extends DepthFirstVisitor {
      public List<FormalParameter> parameters = new ArrayList<>();

      @Override
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
  // FormalParameter objects. Does not include implicit parameters for
  // inner classes.
  public static List<FormalParameter> getParametersNoImplicit(ConstructorDeclaration cd) {
    class GetParametersVisitor extends DepthFirstVisitor {
      public List<FormalParameter> parameters = new ArrayList<>();

      @Override
      public void visit(FormalParameter p) {
        parameters.add(p);
      }
    }
    GetParametersVisitor v = new GetParametersVisitor();
    FormalParameters fp = cd.f2;
    fp.accept(v);
    return v.parameters;
  }

  // Returns the parameters of the constructor, as a list of
  // FormalParameter objects.  Returns an empty list if there are no
  // parameters.
  public static List<FormalParameter> getParameters(ConstructorDeclaration cd) {
    class GetParametersVisitor extends DepthFirstVisitor {
      public List<FormalParameter> parameters = new ArrayList<>();

      @Override
      public void visit(FormalParameter p) {
        parameters.add(p);
      }
    }
    GetParametersVisitor v = new GetParametersVisitor();
    FormalParameters fp = cd.f2;
    fp.accept(v);

    // Inner class constructors have implicit outer class parameter, which had
    // caused the constructor signatures not to match (and thus invariants would
    // not merge into inner class constructors)

    // Look into replacing getClass because that requires that the compiled class
    // be in the classpath

    Node innerClassNode = getParent(ClassOrInterfaceDeclaration.class, cd);
    Node outerClassNode = getParent(ClassOrInterfaceDeclaration.class, innerClassNode);

    //     boolean isNestedStatic = false;
    //     Node nestedMaybe = innerClassNode.getParent();
    //     if (nestedMaybe instanceof NestedClassDeclaration) {
    //       if (isStatic((NestedClassDeclaration)nestedMaybe)) {
    //         isNestedStatic = true;
    //       }
    //     }

    if (isInner((ClassOrInterfaceDeclaration) innerClassNode)
        && !isStatic((ClassOrInterfaceDeclaration) innerClassNode)) {
      NodeToken classNameToken = ((ClassOrInterfaceDeclaration) outerClassNode).f1;
      ClassOrInterfaceType name =
          new ClassOrInterfaceType(classNameToken, new NodeOptional(), new NodeListOptional());
      NodeSequence n10 = new NodeSequence(name);
      NodeSequence n9 = new NodeSequence(n10);
      n9.addNode(new NodeListOptional());
      ReferenceType refType = new ReferenceType(new NodeChoice(n9, 1));
      jtb.syntaxtree.Type type = new jtb.syntaxtree.Type(new NodeChoice(refType, 1));
      // Setting all the line and column info on the NodeTokens is a hassle.
      // So we sidestep and 'cheat' by putting a blank as the first character of the name.
      // This works because we are going to re-parse it in the node Create method.
      VariableDeclaratorId dummyOuterParamName =
          new VariableDeclaratorId(new NodeToken(" outer$this"), new NodeListOptional());
      FormalParameter implicitOuter =
          new FormalParameter(
              new Modifiers(new NodeListOptional()),
              new NodeOptional(),
              type,
              new NodeOptional(),
              dummyOuterParamName);
      v.parameters.add(0, implicitOuter);
    }

    return v.parameters;
  }

  public static void addImport(CompilationUnit u, ImportDeclaration i) {
    u.f1.addNode(i);
  }

  // Returns a list of Strings, the names of all the variables in the node.
  // The node is an expression, conditional expression, or primary
  // expression.
  @SuppressWarnings("JdkObsolete") // JTB uses Enumeration
  public static Set<String> getVariableNames(Node expr) {

    class GetSymbolNamesVisitor extends DepthFirstVisitor {
      public Set<String> symbolNames = new HashSet<>();

      @Override
      public void visit(Name n) {
        Node gp = n.getParent().getParent();
        if (gp instanceof PrimaryPrefix) {
          PrimaryExpression ggp = (PrimaryExpression) gp.getParent();
          for (Enumeration e = getPrimarySuffixes(ggp); e.hasMoreElements(); ) {
            PrimarySuffix s = (PrimarySuffix) e.nextElement();
            if (s.f0.choice instanceof Arguments) {
              return;
            }
          }
          symbolNames.add(format(n));
        }
      }
    }

    GetSymbolNamesVisitor v = new GetSymbolNamesVisitor();
    expr.accept(v);
    return v.symbolNames;
  }

  /**
   * Returns an Enumeration of PrimarySuffix objects (but the static type of the elements is only
   * known to be Node).
   */
  public static Enumeration getPrimarySuffixes(PrimaryExpression p) {
    return p.f1.elements();
  }

  // Return true if the strings are equal, or if abbreviated is a suffix
  // of goal.  This wouldn't be necessary if we did full type resolution.
  static boolean typeMatch(String pptTypeString, String astTypeString) {
    // System.out.println("Comparing " + pptTypeString + " to " + astTypeString);
    if (astTypeString.equals(pptTypeString)) {
      return true;
    }
    // If astTypeString is missing the leading package name, permit a match
    if (pptTypeString.endsWith(astTypeString)
        && (pptTypeString.charAt(pptTypeString.length() - astTypeString.length() - 1) == '.')) {
      return true;
    }
    return false;
  }

  /** Return true if this is the main method for this class. */
  public static boolean isMain(MethodDeclaration md) {
    if (Ast.getName(md).equals("main")) {
      List<FormalParameter> params = Ast.getParameters(md);
      if (params.size() == 1) {
        FormalParameter fp = params.get(0);
        String paramtype = Ast.getType(fp);
        if (Ast.typeMatch("java.lang.String[]", paramtype)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * This code is taken from and modified from daikon.PrintInvariants.print_invariants. The main
   * modification is that instead of printing the invariants, we return a list of them.
   * modifications involve removing code that I don't need here, like printing of debugging info.
   *
   * <p>[[ TODO: instead of duplicating code, you should add this method to PrintInvariants (or
   * wherever it belongs) and have PrintInvariants.print_invariants call it. ]]
   */
  public static List<Invariant> getInvariants(PptTopLevel ppt, PptMap ppt_map) {

    // make names easier to read before printing
    ppt.simplify_variable_names();

    // I could instead sort the PptSlice objects, then sort the invariants
    // in each PptSlice.  That would be more efficient, but this is
    // probably not a bottleneck anyway.
    List<Invariant> invs_vector = new ArrayList<>(ppt.getInvariants());

    Invariant[] invs_array = invs_vector.toArray(new Invariant[0]);
    Arrays.sort(invs_array, PptTopLevel.icfp);

    Global.non_falsified_invariants += invs_array.length;

    List<Invariant> accepted_invariants = new ArrayList<>();

    for (int i = 0; i < invs_array.length; i++) {
      Invariant inv = invs_array[i];

      assert !(inv instanceof Equality);
      for (int j = 0; j < inv.ppt.var_infos.length; j++) {
        assert !inv.ppt.var_infos[j].missingOutOfBounds()
            : "var '" + inv.ppt.var_infos[j].name() + "' out of bounds in " + inv.format();
      }
      InvariantFilters fi = InvariantFilters.defaultFilters();

      boolean fi_accepted = true;
      InvariantFilter filter_result = fi.shouldKeep(inv);
      fi_accepted = (filter_result == null);

      // Never print the guarding predicates themselves, they should only
      // print as part of GuardingImplications
      if (fi_accepted && !inv.isGuardingPredicate) {
        Global.reported_invariants++;
        accepted_invariants.add(inv);
      }
    }

    accepted_invariants = InvariantFilters.addEqualityInvariants(accepted_invariants);

    return accepted_invariants;
  }

  public static boolean isStatic(ClassOrInterfaceDeclaration n) {
    return isStaticInternal((Node) n);
  }

  public static boolean isStatic(MethodDeclaration n) {
    return isStaticInternal((Node) n);
  }

  public static Modifiers getModifiers(ClassOrInterfaceDeclaration n) {
    return getModifiersInternal((Node) n);
  }

  public static Modifiers getModifiers(MethodDeclaration n) {
    return getModifiersInternal((Node) n);
  }

  private static Modifiers getModifiersInternal(Node n) {

    assert (n instanceof MethodDeclaration) || (n instanceof ClassOrInterfaceDeclaration);

    ClassOrInterfaceBodyDeclaration decl =
        (ClassOrInterfaceBodyDeclaration) getParent(ClassOrInterfaceBodyDeclaration.class, n);
    assert decl != null;
    NodeSequence seq = (NodeSequence) decl.f0.choice;
    return (Modifiers) seq.elementAt(0);
  }

  private static boolean isStaticInternal(Node n) {
    // Grammar production for ClassOrInterfaceBodyDeclaration
    // f0 -> Initializer()
    //       | Modifiers() ( ClassOrInterfaceDeclaration(modifiers) | EnumDeclaration(modifiers) |
    // ConstructorDeclaration() | FieldDeclaration(modifiers) | MethodDeclaration(modifiers) )
    //       | ";"
    assert (n instanceof MethodDeclaration) || (n instanceof ClassOrInterfaceDeclaration);

    Modifiers modifiers = getModifiersInternal(n);
    if (modifierPresent(modifiers, "static")) {
      return true;
    } else {
      return false;
    }
  }

  public static boolean modifierPresent(Modifiers modifiers, String modifierString) {
    // Grammar production:
    // f0 -> ( ( "public" | "static" | "protected" | "private" | "final" | "abstract" |
    // "synchronized" | "native" | "transient" | "volatile" | "strictfp" | Annotation() ) )*

    NodeListOptional list = modifiers.f0;
    for (int i = 0; i < list.size(); i++) {
      NodeChoice nodeChoice = (NodeChoice) list.elementAt(i);

      if (nodeChoice.choice instanceof NodeToken) {
        NodeToken keyword = (NodeToken) nodeChoice.choice;
        if (keyword.toString().equals(modifierString)) {
          return true;
        }
      }
    }
    return false;
  }

  public static boolean isInner(ClassOrInterfaceDeclaration n) {
    if (getParent(ClassOrInterfaceDeclaration.class, n) != null) {
      return true;
    } else {
      return false;
    }
  }

  public static boolean isInAnonymousClass(Node node) {
    ClassOrInterfaceBody clsbody =
        (ClassOrInterfaceBody) Ast.getParent(ClassOrInterfaceBody.class, node);
    return !(clsbody.getParent() instanceof ClassOrInterfaceDeclaration);
  }

  public static boolean isInterface(ClassOrInterfaceBody n) {
    // n.getParent won't be an ClassOrInterfaceDeclaration for anonymous
    // class bodies such as in  new List() { ... }  -- anonymous classes can
    // never be interfaces, however, so that's simple enough to handle. :)
    if (!(n.getParent() instanceof ClassOrInterfaceDeclaration)) {
      return false;
    }

    return isInterface((ClassOrInterfaceDeclaration) n.getParent());
  }

  public static boolean isInterface(ClassOrInterfaceDeclaration n) {
    // Grammar production for ClassOrInterfaceDeclaration:
    // f0 -> ( "class" | "interface" )
    // f1 -> <IDENTIFIER>
    // f2 -> [ TypeParameters() ]
    // f3 -> [ ExtendsList(isInterface) ]
    // f4 -> [ ImplementsList(isInterface) ]
    // f5 -> ClassOrInterfaceBody(isInterface)
    NodeToken t = (NodeToken) n.f0.choice;
    String token = t.tokenImage;
    if (token.equals("interface")) {
      return true;
    } else {
      return false;
    }
  }

  public static boolean isPrimitive(jtb.syntaxtree.Type n) {
    // Grammar production:
    // f0 -> ReferenceType()
    //       | PrimitiveType()
    return (n.f0.choice instanceof PrimitiveType);
  }

  public static boolean isReference(jtb.syntaxtree.Type n) {
    // Grammar production:
    // f0 -> ReferenceType()
    //       | PrimitiveType()
    return (n.f0.choice instanceof ReferenceType);
  }

  public static boolean isArray(jtb.syntaxtree.Type n) {
    // Grammar production:
    // f0 -> PrimitiveType() ( "[" "]" )+
    //       | ( ClassOrInterfaceType() ) ( "[" "]" )*
    if (isPrimitive(n)) {
      return false;
    }
    assert isReference(n);
    ReferenceType refType = (ReferenceType) n.f0.choice;
    NodeSequence seq = (NodeSequence) refType.f0.choice;
    if (seq.elementAt(0) instanceof PrimitiveType) {
      return true; // see grammar--must be array in this case
    }
    NodeListOptional opt = (NodeListOptional) seq.elementAt(1);
    return opt.present();
  }

  public static String classnameForSourceOutput(Class<?> c) {

    assert !c.equals(Void.TYPE);

    if (c.isPrimitive()) {
      return c.getName();
    } else if (c.isArray()) {
      return SignaturesUtil.classGetNameToBinaryName(c.getName());
    } else {
      return c.getName();
    }
  }
}
