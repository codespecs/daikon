package daikon.tools.jtb;

import java.util.*;
import java.io.*;
import jtb.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import utilMDE.*;
import daikon.Global;

/**
 * Given a class with certain methods annotated as observers, adds one
 * field for each method, and adds code to update those fields with
 * the value of the observer at every procedure entry and exit.
 **/
public class InstrumentObserversVisitor
  extends DepthFirstVisitor
{

  public InstrumentObserversVisitor(Collection observers // [MethodDeclaration]
				    )
  {
    observer_methods = new ArrayList(observers);
  }

  // All methods that we want to observe
  private final List observer_methods;

  /**
   * Add fields for each observed method
   **/
  public void visit(ClassBody clazz) {
    super.visit(clazz);

    for (Iterator i = observer_methods.iterator(); i.hasNext(); ) {
      MethodDeclaration method = (MethodDeclaration) i.next();
      String fldtype = getFieldTypeFor(method);
      String fldname = getFieldNameFor(method);
      String code = "private " + fldtype + " " + fldname + ";" + Global.lineSep;
      ClassBodyDeclaration decl =
	(ClassBodyDeclaration) Ast.create("ClassBodyDeclaration", code);
      Ast.addDeclaration(clazz, decl);
    }
  }

  // Methods that we have created
  private final Set generated_methods = new HashSet();
  /**
   * Renames the original method, and adds a new one that wraps it.
   **/
  public void visit(MethodDeclaration method) {
    super.visit(method);

    if (generated_methods.contains(method)) {
      return;
    }

    // Create the wrapper method as a copy of the original
    MethodDeclaration wrapper = (MethodDeclaration)
      Ast.copy("MethodDeclaration", method);

    // Change the body of the wrapper to call the original
    String name = Ast.getName(method);
    String returnType = Ast.getReturnType(method);
    String maybeReturn = (returnType.equals("void") ? "" : "return");
    Vector parameters = new Vector();
    for (Iterator i = Ast.getParameters(method).iterator(); i.hasNext(); ) {
      FormalParameter param = (FormalParameter) i.next();
      parameters.add(Ast.getName(param));
    }

    StringBuffer body = new StringBuffer();
    body.append("{");
    for (Iterator i = observer_methods.iterator(); i.hasNext(); ) {
      MethodDeclaration obs_method = (MethodDeclaration) i.next();
      String fldname = getFieldNameFor(obs_method);
      body.append("  " + fldname + " = " + getCallExprFor(obs_method) + ";");
    }
    body.append("  try {");
    body.append("    " + maybeReturn + " $" + name + "(" + UtilMDE.join(parameters, ", ") + ");");
    body.append("  } finally {");
    for (Iterator i = observer_methods.iterator(); i.hasNext(); ) {
      MethodDeclaration obs_method = (MethodDeclaration) i.next();
      String fldname = getFieldNameFor(obs_method);
      body.append("    " + fldname + " = " + getCallExprFor(obs_method) + ";");
    }
    body.append("  }");
    body.append("}");

    Ast.setBody(wrapper, body.toString());
    wrapper.accept(new TreeFormatter(2, 0));

    ClassBody c = (ClassBody) Ast.getParent(ClassBody.class, method);
    ClassBodyDeclaration d = (ClassBodyDeclaration)
      Ast.create("ClassBodyDeclaration", Ast.print(wrapper));
    Ast.addDeclaration(c, d);
    MethodDeclaration generated_method = (MethodDeclaration) d.f0.choice;
    generated_methods.add(generated_method);

    // Rename the original implementation, and make it private
    Ast.setName(method, "$" + name);
    Ast.setAccess(method, "private");
  }

  private String getFieldTypeFor(MethodDeclaration method) {
    String type = Ast.getReturnType(method);
    // TODO: handle Iterator, etc.
    return type;
  }

  private String getFieldNameFor(MethodDeclaration method) {
    String name = Ast.getName(method);
    if (name.charAt(0) == '$') {
      // we renamed the method, but we want the original name
      name = name.substring(1);
    }
    // TODO: handle overloading (?), etc.
    return "$method$" + name;
  }

  private String getCallExprFor(MethodDeclaration method) {
    String name = Ast.getName(method);
    if (name.charAt(0) != '$') {
      name = "$" + name;
    }
    return name + "()";
  }

  /**
   * Constructs a list of "@ obvserver"-annotated methods.
   **/
  public static final class GrepObserversVisitor
    extends DepthFirstVisitor
  {
    public final List observer_methods = new ArrayList();

    /**
     * Note all occurences of observer methods.
     **/
    public void visit(NodeToken n) {
      super.visit(n);

      if (n.numSpecials() == 0) { return; }
      for (Enumeration e = n.specialTokens.elements(); e.hasMoreElements(); ) {
	String comment = ((NodeToken) e.nextElement()).tokenImage;
	if (comment.indexOf("@ observer") >= 0) {
	  MethodDeclaration method =
	    (MethodDeclaration) Ast.getParent(MethodDeclaration.class, n);
	  Assert.assert(method != null);
	  String name = Ast.getName(method);
	  String returnType = Ast.getReturnType(method);
	  if (returnType.equals("void")) {
	    System.err.println("Warning: skipping void observer " + name);
	    return;
	  }
	  // System.out.println("Found name: " + name);
	  observer_methods.add(method);
	}
      }
    }
  }

  public static void main(String[] args)
    throws IOException
  {
    for (int i=0; i < args.length; i++) {
      String javafile = args[i];

      if (! javafile.endsWith(".java")) {
        System.out.println("File does not end in .java: " + javafile);
        System.exit(1);
      }

      System.out.println("Processing file " + javafile);
      Reader input = new FileReader(javafile);
      Writer output = new OutputStreamWriter(System.out);
      // File outputFile = new File(javafile + "-obsfields");
      // outputFile.getParentFile().mkdirs();
      // Writer output = new FileWriter(outputFile);

      JavaParser parser = new JavaParser(input);
      Node root = null;
      try {
	root = parser.CompilationUnit();
      } catch (ParseException e) {
	e.printStackTrace();
	System.exit(1);
      }

      GrepObserversVisitor grep = new GrepObserversVisitor();
      root.accept(grep);
      root.accept(new TreeFormatter(2, 80));

      InstrumentObserversVisitor instrument =
	new InstrumentObserversVisitor(grep.observer_methods);
      root.accept(instrument);
      root.accept(new TreeFormatter(2, 80));

      root.accept(new TreeDumper(output));
    }
  }

}
