// See MergeESC for documentation of behavior.

package daikon.tools.jtb;

import java.util.*;
import java.io.*;
import junit.framework.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.*;
import utilMDE.Assert;
import utilMDE.ArraysMDE;
import utilMDE.UtilMDE;
import daikon.inv.Invariant;
import daikon.inv.unary.sequence.EltNonZero;
import daikon.inv.unary.stringsequence.EltOneOfString;
import daikon.inv.unary.stringsequence.OneOfStringSequence;


// For each class:  (UnmodifiedClassDeclaration)
//  * insert object invariants
//  * insert owner assertions for fields
// For each field:
//  * add spec_public
// For each method:
//  * add preconditions
//  * add postconditions (including exsures)
//  * add "modifies"
//  * all of these may be prefixed by "also_"
// For each field assignment:
//  * add owner annotations




class MergeESCVisitor extends DepthFirstVisitor {

  public final static String lineSep = System.getProperty("line.separator");

  public final static String JML_START_COMMENT = "/*@" + lineSep;
  public final static String JML_END_COMMENT = "@*/" + lineSep;

  public PptMap ppts;
  public boolean slashslash;       // whether to use // or /* style comments
  public boolean insert_inexpressible; // whether to insert annotations not supported by ESC
  public boolean lightweight;      // whether to use full JML specs or lightweight ESC specs instead

  public Vector addedComments = new Vector(); // elements are NodeTokens

  private String[] ownedFieldNames;  // list of fields in this and related classes
  private String[] finalFieldNames;  // list of fields in this and related classes
  private String[] notContainsNullFieldNames;  // list of fields in this and related classes
  private HashMap elementTypeFieldNames; // list of fields in this and related classes


  public MergeESCVisitor(PptMap ppts, boolean slashslash, boolean insert_inexpressible) {
    super();
    initialize(ppts, slashslash, insert_inexpressible, false);
  }

  public MergeESCVisitor(PptMap ppts, boolean slashslash, boolean insert_inexpressible, boolean lightweight) {
    initialize(ppts, slashslash, insert_inexpressible, lightweight);
  }

  private void initialize(PptMap ppts, boolean slashslash, boolean insert_inexpressible, boolean lightweight) {
    this.ppts = ppts;
    this.slashslash = slashslash;
    this.insert_inexpressible = insert_inexpressible;
    this.lightweight = lightweight;
  }

  // Like Ast.addComment, but also keeps a list of what comments were added.
  void addComment(Node n, String comment, boolean first) {
    NodeToken nt = new NodeToken(comment);
    Ast.addComment(n, nt, first);
    addedComments.add(nt);
  }

  // Like Ast.addComment, but also keeps a list of what comments were added.
  void addComment(Node n, String comment) {
    addComment(n, comment, false);
  }

  // Like Ast.addComment, but also keeps a list of what comments were added.
  void addCommentAfter(Node n, String comment) {
    addComment(Ast.nodeTokenAfter(n), comment, true);
  }

  private boolean isOwned(String fieldname) {
    return (ArraysMDE.indexOf(ownedFieldNames, fieldname) != -1);
  }

  private boolean isFinal(String fieldname) {
    return (ArraysMDE.indexOf(finalFieldNames, fieldname) != -1);
  }

  private boolean isNotContainsNull(String fieldname) {
    return (ArraysMDE.indexOf(notContainsNullFieldNames, fieldname) != -1);
  }

  private boolean isElementType(String fieldname) {
    return elementTypeFieldNames.containsKey(fieldname);
  }

  private String elementType(String fieldname) {
    return (String)elementTypeFieldNames.get(fieldname);
  }

  // ClassDeclaration is a top-level (non-nested) construct.  Collect all
  // the fields in this and any nested class, so that we can recognize
  // assignments to them later on.
  /**
   * f0 -> ( "abstract" | "final" | "public" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
  public void visit(ClassDeclaration n) {
    super.visit(n);             // call "accept(this)" on each field
  }

  /**
   * f0 -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
  public void visit(NestedClassDeclaration n) {
    super.visit(n);             // call "accept(this)" on each field
  }

  // Insert object invariants for this class.
  // Insert owner assertions for fields.
  /**
   * f0 -> "class"
   * f1 -> <IDENTIFIER>
   * f2 -> [ "extends" Name() ]
   * f3 -> [ "implements" NameList() ]
   * f4 -> ClassBody()
   */
  public void visit(UnmodifiedClassDeclaration n) {
    String classname = Ast.getClassName(n);
    String pptname = classname + ":::OBJECT";
    PptTopLevel object_ppt = ppts.get(pptname);
    if (object_ppt == null) {
      pptname = classname + ":::CLASS";
      object_ppt = ppts.get(pptname);
    }

    // Store and restore field names because we must deal with
    // visiting inner classes (which have their own fields)
    String[] old_owned = ownedFieldNames;
    String[] old_final = finalFieldNames;
    String[] old_notContainsNull = notContainsNullFieldNames;
    HashMap old_elementType = elementTypeFieldNames;
    { // set fieldNames slots
      CollectFieldsVisitor cfv = new CollectFieldsVisitor();
      n.accept(cfv);
      ownedFieldNames = cfv.ownedFieldNames();
      finalFieldNames = cfv.finalFieldNames();
      if (object_ppt == null) {
        notContainsNullFieldNames = new String[0];
        elementTypeFieldNames = new HashMap();
      } else {
        notContainsNullFieldNames = not_contains_null_fields(object_ppt, cfv);
        elementTypeFieldNames = element_type_fields(object_ppt, cfv);
      }
    }

    super.visit(n);             // call "accept(this)" on each field

    if (lightweight)
      for (int i=ownedFieldNames.length-1; i>=0; i--) {
        addComment(n.f4.f1, javaLineComment("@ invariant " + ownedFieldNames[i] + ".owner == this;"), true);
    }
    if (object_ppt == null) {
      // System.out.println("No object program point found for " + classname);
    } else {
      String[] obj_invs = Ast.invariants_for(object_ppt, ppts);
      insertInvariants(n.f4.f1, "invariant", obj_invs);
    }

    ownedFieldNames = old_owned;
    finalFieldNames = old_final;
    notContainsNullFieldNames = old_notContainsNull;
    elementTypeFieldNames = old_elementType;
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  public void visit(FieldDeclaration n) {
    super.visit(n);             // call "accept(this)" on each field

    if (! Ast.contains(n.f0, "public")) {
      addComment(n, "/*@ spec_public */ ");
    }
  }

  // Node n is a MethodDeclaration or a ConstructorDeclaration
  String[][] get_requires_and_ensures(PptMap ppts, Node n) {
    String[] requires_invs = null;
    String[] ensures_invs = null;

    Vector matching_ppts = Ast.getMatches(ppts, n);
    for (Iterator itor = matching_ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      String prefix;
      if (ppt.ppt_name.isEnterPoint()) {
        requires_invs = Ast.invariants_for(ppt, ppts);
      } else if (ppt.ppt_name.isExitPoint()) {
        if (! ppt.ppt_name.isCombinedExitPoint()) {
          continue;
        }
        ensures_invs = Ast.invariants_for(ppt, ppts);
      }
    }

    return new String[][] { requires_invs, ensures_invs };
  }

  HashMap get_exceptions(PptMap ppts, Node n) {
    HashMap result = new HashMap();

    Vector matching_ppts = Ast.getMatches(ppts, n);
    for (Iterator itor = matching_ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      String prefix;
      if (ppt.ppt_name.isThrowsPoint()) {
	String exceptionName = "Not getting called"; // ppt.ppt_name.dontKnowHowToDoThis();
	Collection exceptionInvariants;

	if (result.containsValue(exceptionName)) {
	  exceptionInvariants = (Collection)result.get(exceptionName);
	  exceptionInvariants.add(ppt.getInvariants());
	} else {
	  exceptionInvariants = new Vector(ppt.getInvariants());
	  result.put(exceptionName,exceptionInvariants);
	}
      }
    }

    return result;
  }

  public void insertAlso(Node n) {
    addComment(n, "@ also" + lineSep, true);
  }

  // n must be a MethodDeclaration or ConstructorDeclaration
  public void insertBehavior(Node n, boolean noExceptions, boolean noEnsures) {
    class InsertBehaviorVisitor extends DepthFirstVisitor {
      Node n;
      boolean noExceptions, noEnsures, behaviorInserted;

      public InsertBehaviorVisitor(Node n, boolean noExceptions, boolean noEnsures) {
	super();
	this.n = n;
	this.noExceptions = noExceptions;
	this.noEnsures = noEnsures;
	behaviorInserted = false;
      }

      private String getBehaviorString() {
	return (noExceptions ? "normal_behavior" :
		(noEnsures ? "exceptional_behavior" : "behavior"));
      }

      public void visit(NodeChoice nc) {
        // System.out.println("InsertBehavior visitor visiting a NodeChoice");
	String modifier = (nc != null && nc.choice != null ? nc.choice.toString() : "");
	// System.out.println("A node choice here: " + modifier);
	if (Ast.isAccessModifier(modifier)) {
	  addComment(n, "@ " + modifier + " " + getBehaviorString() + lineSep, true);
	  behaviorInserted = true;
	}
      }

      public void visit(NodeOptional no) {
	// System.out.println("InsertBehavior visitor visiting a NodeOptional");
	visit((NodeChoice)no.node);
      }

      public void visit(NodeListOptional nlo) {
	// System.out.println("InsertBehavior visitor visiting a NodeListOptional");
	// System.out.println("With " + nlo.nodes.size() + " nodes");
	for (int i=0; i<nlo.nodes.size() && !behaviorInserted; i++) {
	  // System.out.println("Visiting a NodeChoice");

	  // The other way around (that is, ((NodeChoice)nlo...).accept(this))
	  // does not work because of the way NodeChoice.accept is defined
	  visit((NodeChoice)nlo.nodes.get(i));
	}
	if (!behaviorInserted)
	  addComment(n, "@ " + "protected " + getBehaviorString() + lineSep, true);
      }

      public void visit(MethodDeclaration md) {
	// System.out.println("InsertBehavior visitor visiting a MethodDeclaration");
	md.f0.accept(this);
      }

      public void visit(ConstructorDeclaration cd) {
	// System.out.println("InsertBehavior visitor visiting a ConstructorDeclaration");
	cd.f0.accept(this);
      }
    }

    InsertBehaviorVisitor v = new InsertBehaviorVisitor(n,noExceptions,noEnsures);
    if (n instanceof MethodDeclaration)
      ((MethodDeclaration)n).accept(v);
    else
      ((ConstructorDeclaration)n).accept(v);
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "abstract" | "final" | "native" | "synchronized" )*
   * f1 -> ResultType()
   * f2 -> MethodDeclarator()
   * f3 -> [ "throws" NameList() ]
   * f4 -> ( Block() | ";" )
   */
  public void visit(MethodDeclaration n) {
    // System.out.println("MethodDeclaration: " + n.f2.f0);

    super.visit(n);             // call "accept(this)" on each field

    String[][] requires_and_ensures = get_requires_and_ensures(ppts, n);
    String[] requires_invs = requires_and_ensures[0];
    String[] ensures_invs = requires_and_ensures[1];

    HashMap exceptions = get_exceptions(ppts, n);

    // Special case for main method:  add "arg != null" and
    // "\nonnullelements(arg)".
    if (Ast.isMain(n)) {
      if (requires_invs == null) {
        requires_invs = new String[0];
      }
      int old_size = requires_invs.length;
      String[] new_requires_invs = new String[old_size+2];
      System.arraycopy(requires_invs, 0, new_requires_invs, 0, old_size);
      requires_invs = new_requires_invs;
      // should really only add these if they aren't already present
      FormalParameter fp = (FormalParameter) Ast.getParameters(n).get(0);
      String param = Ast.getName(fp);
      requires_invs[old_size] = param + " != null";
      requires_invs[old_size+1] = "\\nonnullelements(" + param + ")";
    }

    String ensures_tag = "ensures";
    String requires_tag = "requires";
    boolean isOverride = Ast.isOverride(n); // of a superclass
    boolean isImplementation = Ast.isImplementation(n); // of an interface

    if (lightweight) {
      if (isImplementation) {
	requires_tag = "also_" + requires_tag;
	ensures_tag = "also_" + ensures_tag;
      }
      if (isOverride) {
	requires_invs = null; // no requires permitted on overridden methods
	ensures_tag = "also_" + ensures_tag;
      }
    }

    if (!lightweight)
      addComment(n, JML_END_COMMENT, true);

    boolean invariantInserted =
      insertInvariants(n, ensures_tag, ensures_invs, lightweight);

    invariantInserted =
      insertInvariants(n, requires_tag, requires_invs, lightweight) ||
      invariantInserted;

    if (!lightweight) {
      if (!invariantInserted)
        insertJMLWorkaround(n);
      insertBehavior(n,
                     (exceptions != null ? exceptions.isEmpty() : true),
                     (ensures_invs != null ? (ensures_invs.length == 0) : true));
      if (isImplementation || isOverride) {
	insertAlso(n);
      }
      addComment(n, JML_START_COMMENT, true);
    }

    //if (!exceptions.isEmpty())
    //  insertExceptions(n, exceptions);
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
    // System.out.println("ConstructorDeclaration: " + n.f1);

    super.visit(n);             // call "accept(this)" on each field

    String[][] requires_and_ensures = get_requires_and_ensures(ppts, n);
    String[] requires_invs = requires_and_ensures[0];
    String[] ensures_invs = requires_and_ensures[1];

    HashMap exceptions = get_exceptions(ppts, n);

    if (!lightweight)
      addComment(n, JML_END_COMMENT, true);

    boolean invariantInserted =
      insertInvariants(n, "ensures", ensures_invs, lightweight);

    invariantInserted =
      insertInvariants(n, "requires", requires_invs, lightweight) ||
      invariantInserted;

    if (!lightweight) {
      if (!invariantInserted)
        insertJMLWorkaround(n);
      insertBehavior(n, exceptions.isEmpty(), ensures_invs == null || ensures_invs.length == 0);
      addComment(n, JML_START_COMMENT, true);
    }

  }

  public void insertJMLWorkaround(Node n) {
    addComment(n, "@ requires true;" + lineSep, true);
  }

  public boolean insertInvariants(Node n, String prefix, String[] invs) {
    return insertInvariants(n, prefix, invs, true);
  }

  // The "invs" argument may be null, in which case no work is done.
  public boolean insertInvariants(Node n, String prefix, String[] invs, boolean useJavaComment) {
    if (invs == null) {
      return false;
    }

    boolean invariantInserted = false;

    for (int i=invs.length-1; i>=0; i--) {
      String inv = invs[i];
      if (inv.startsWith("      Unmodified variables: ")
          || inv.startsWith("      Modified variables: ")
          || inv.startsWith("      Modified primitive arguments: ")) {
        // not an invariant
        continue;
      } else if ((inv.indexOf(".format_esc() needs to be implemented: ") != -1)
                 || (inv.indexOf('~') != -1)
                 || (inv.indexOf("\\new") != -1)
		 || (inv.indexOf(".toString ") != -1)
		 || (inv.endsWith(".toString"))
                 || (inv.indexOf("warning: method") != -1)
		 || (inv.indexOf("inexpressible") != -1)) {
        // inexpressible invariant
        if (insert_inexpressible) {
          addComment(n, javaLineComment("! " + inv + ";"), true);
        }
        continue;
      } else if (inv.startsWith("modifies ") || inv.startsWith("assignable ")) {
        if (lightweight && prefix.startsWith("also_")) {
          inv = "also_" + inv;
        }
	String commentContents = "@ " + inv + ";";
	if (useJavaComment)
	  commentContents = javaLineComment(commentContents);
        else
          commentContents += lineSep;
        addComment(n, commentContents, true);
        invariantInserted = true;
      } else {
	String commentContents = "@ " + prefix + " " + inv + ";";
	if (useJavaComment)
	  commentContents = javaLineComment(commentContents);
        else
          commentContents += lineSep;

        addComment(n, commentContents, true);
        invariantInserted = true;
      }
    }
    return invariantInserted;
  }

  // Set .owner and/or .containsnull for ++, --, etc expressions within a
  // statement.
  /**
   * f0 -> PreIncrementExpression()
   *       | PreDecrementExpression()
   *       | PrimaryExpression() [ "++" | "--" | AssignmentOperator() Expression() ]
   */
  public void visit(StatementExpression n) {
    super.visit(n);             // call "accept(this)" on each field

    // System.out.println("Found a statement expression: " + n.f0.choice);

    if (n.f0.choice instanceof NodeSequence) {
      NodeSequence ns = (NodeSequence) n.f0.choice;
      PrimaryExpression pe = (PrimaryExpression) ns.elementAt(0);
      // for (int i=0; i<ns.size(); i++) {
      //   System.out.println("ns #" + i + ": " + ns.elementAt(i));
      // }
      if (ns.size() == 2) {
        NodeOptional no = (NodeOptional) ns.elementAt(1);
        NodeChoice nc = (NodeChoice) no.node;
        if ((nc != null) && (nc.choice instanceof NodeSequence)) {
          // It's an assignment.

          // Don't take action unless the PrimaryExpression is a simple
          // Name (that's effectively checked below) and has no
          // PrimarySuffix or else its prefix is "this" (check that here).
          String fieldname = null;
          if (pe.f1.size() == 0) {
            fieldname = Ast.fieldName(pe);
          } else if (pe.f1.size() == 1) {
            if (pe.f0.f0.which == 1) { // prefix is "this"
              PrimarySuffix ps = (PrimarySuffix) pe.f1.elementAt(0);
              if (ps.f0.which == 3) { // suffix is an identifier
                NodeSequence ns2 = (NodeSequence) ps.f0.choice;
                fieldname = ((NodeToken) ns2.elementAt(1)).tokenImage;
              }
            }
          }

          if (fieldname != null) {
            // System.out.println("In statement, fieldname = " + fieldname);
            if ((fieldname != null)
                && (isOwned(fieldname)
                    || isNotContainsNull(fieldname)
                    || isElementType(fieldname))) {
              ConstructorDeclaration cd
                = (ConstructorDeclaration) Ast.getParent(ConstructorDeclaration.class, n);
              MethodDeclaration md
                = (MethodDeclaration) Ast.getParent(MethodDeclaration.class, n);
              if ((cd != null)
                  || ((md != null) && (! Ast.contains(md.f0, "static")))) {
                Node parent = Ast.getParent(Statement.class, n);
                // If parent isn't in a block (eg, if parent
                // is sole element in then or else clause), then this is wrong.
                // It's safe, however.  But does it cause syntax errors if an
                // else clause follows a then clause without braces?
                if (isOwned(fieldname)) {
                  if (lightweight)
                    addCommentAfter(parent, javaLineComment("@ set " + fieldname + ".owner = this;"));
                }
                if (isNotContainsNull(fieldname)) {
                  addCommentAfter(parent, javaLineComment("@ set " + fieldname + ".containsNull = false;"));
                }
                if (isElementType(fieldname)) {
                  addCommentAfter(parent, javaLineComment("@ set " + fieldname + ".elementType = " + elementType(fieldname) + ";"));
                }
              }
            }
          }
        }
      }
    }
  }

  // This is an assignment exactly if field f1 is present.
  /**
   * f0 -> ConditionalExpression()
   * f1 -> [ AssignmentOperator() Expression() ]
   */
  public void visit(Expression n) {
    super.visit(n);             // call "accept(this)" on each field

    if (n.f1.present()) {
      // it's an assignment
      // System.out.println("found an assignment expression: " + n);
      PrimaryExpression pe = Ast.assignment2primaryexpression(n);
      String fieldname = Ast.fieldName(pe);
      // System.out.println("In expression, fieldname = " + fieldname);
      Node stmt = Ast.getParent(Statement.class, n);
      if ((fieldname != null) && isOwned(fieldname)) {
        if (lightweight)
          addCommentAfter(stmt, javaLineComment("@ set " + fieldname + ".owner = this;"));
      }

    }

  }



///////////////////////////////////////////////////////////////////////////
/// Below this line all is cut-and-paste from FreqVisitor (or some such)
///

//   public void visit(MethodDeclaration method) {
//     if (shouldInstrument(method)) {
//       methodFreq(method);
//     }
//   }
//
//   protected static boolean shouldInstrument(MethodDeclaration method) {
//     String returnType = Ast.getReturnType(method);
//     List parameters = Ast.getParameters(method);
//     return (!returnType.equals("void") && parameters.size() != 0);
//   }
//
//   private static void methodFreq(MethodDeclaration method) {
//     MemoVisitor.addImports(method);
//     MemoVisitor.addCache(method, new MemoOptions());
//     insertCacheUpdates(method);
//   }
//
//   private static void insertCacheUpdates(MethodDeclaration method) {
//     MemoVisitor.w = new StringWriter();
//     MemoVisitor.p = new PrintWriter(MemoVisitor.w);
//
//     pl("{");
//     initializeCache(method);
//     pl("List $key;");
//     MemoVisitor.createLookupKey(method, true);
//     incrementFrequency(method);
//
//     String oldBody = Ast.getBody(method);
//
//     // Remove the first "{"
//     int i = oldBody.indexOf('{');
//     oldBody = oldBody.substring(i+1);
//
//     p(oldBody);
//     Ast.setBody(method, MemoVisitor.w.toString());
//   }
//
//   private static void initializeCache(MethodDeclaration method) {
//     String cache = MemoVisitor.getCacheName(method);
//     String fullName = Ast.getFullName(method);
//
//     pl("if (" + cache + "== null) {");
//     pl(cache + "= new HashMap();");
//     pl("Thread $printCacheThread = new Thread() {");
//     pl("public void run() {");
//     pl("synchronized(Thread.class) {");
//     pl("SortedMap $m = new TreeMap();");
//     pl("for (Iterator $i = " + cache +
//        ".keySet().iterator(); $i.hasNext(); ) {");
//     pl("Object $key = $i.next();");
//     pl("Object $value = " + cache + ".get($key);");
//     pl("if ($m.containsKey($value)) {");
//     pl("int $oldFreqOfFreq = ((Integer) $m.get($value)).intValue() + 1;");
//     pl("$m.put($value, new Integer($oldFreqOfFreq + 1));");
//     pl("} else {");
//     pl("$m.put($value, new Integer(1));");
//     pl("}");
//     pl("}");
//     pl("System.out.println();");
//     pl("System.out.println(\"Frequencies for method " + fullName + "\");");
//     pl("while (!$m.isEmpty()) {");
//     pl("Object $key = $m.lastKey();");
//     pl("Object $value = $m.get($key);");
//     pl("System.out.println($key + \"\t\" + $value);");
//     pl("$m.remove($key);");
//     pl("}");
//     pl("}}};");
//     pl("Runtime.getRuntime().addShutdownHook($printCacheThread);");
//     pl("}");
//   }
//
//   private static void incrementFrequency(MethodDeclaration method) {
//     String cache = MemoVisitor.getCacheName(method);
//
//     pl("Integer $hash = new Integer($key.hashCode());");
//     pl("if (" + cache + ".containsKey($hash)) {");
//     pl("int $oldFreq = ((Integer) " + cache + ".get($hash)).intValue();");
//     pl(cache + ".put($hash, new Integer($oldFreq + 1));");
//     pl("} else {");
//     pl(cache + ".put($hash, new Integer(1));");
//     pl("}");
//   }
//
//   // Convenience methods to provide more concise syntax
//   private static void p(String s) {
//     MemoVisitor.p.print(s);
//   }
//   private static void pl(String s) {
//     MemoVisitor.p.println(s);
//   }
// }


  ///////////////////////////////////////////////////////////////////////////
  /// Subroutines
  ///

  /** The argument should already contain "@" or any other leading characters. */
  String javaLineComment(String comment) {
    if (slashslash) {
      return "//" + comment + lineSep;
    } else {
      return "/*" + comment + " */" + lineSep;
    }
  }

  /** The argument should already contain "@" or any other leading characters. */
  String javaComment(String comment) {
    return "/*" + comment + "*/";
  }


  // Returns a list of fields with ".containsNull == false" invariants.
  // ppt is an :::OBJECT or :::CLASS program point.
  String[] not_contains_null_fields(PptTopLevel ppt, CollectFieldsVisitor cfv) {
    // System.out.println("not_contains_null_fields(" + ppt + ")");
    Vector result = new Vector();
    String[] fields = cfv.allFieldNames();
    for (int i=0; i<fields.length; i++) {
      String field = fields[i];
      // System.out.println("field: " + field);
      String varname;
      if (ppt.ppt_name.isObjectInstanceSynthetic()) // ":::OBJECT"
        varname = "this." + field;
      else if (ppt.ppt_name.isClassStaticSynthetic()) // ":::CLASS"
        varname = ppt.ppt_name.getFullClassName() + "." + field;
      else
        throw new Error("Bad ppt: " + ppt);
      VarInfo vi = ppt.findVar(varname);
      if (vi == null) {
        // This happens, for example, for final static vars (see
        // REP_SCALE_FACTOR in MapQuick1/GeoPoint.java).
        System.out.println("Warning: MergeESC: skipping Variable " + varname + " at " + ppt);
      } else {
        Assert.assertTrue(vi != null);
        PptSlice1 slice = ppt.findSlice(vi);
        if (slice != null) {
          EltNonZero enz = EltNonZero.find(slice);
          if (enz != null) {
            String enz_format = format((Invariant)enz);
            if (enz_format.endsWith(".containsNull == false")) {
              result.add(field);
            }
          }
        }
      }
    }
    return (String[]) result.toArray(new String[0]);
  }

  // Returns a HashMap fields with ".elementType == \type(...)" invariants,
  // mapping the field to the type.
  // ppt is an :::OBJECT or :::CLASS program point.
  HashMap element_type_fields(PptTopLevel ppt, CollectFieldsVisitor cfv) {
    // System.out.println("not_contains_null_fields(" + ppt + ")");
    HashMap result = new HashMap();
    String[] fields = cfv.allFieldNames();
    for (int i=0; i<fields.length; i++) {
      String field = fields[i];
      // System.out.println("field: " + field);
      String varname;
      if (ppt.ppt_name.isObjectInstanceSynthetic()) // ":::OBJECT"
        varname = "this." + field;
      else if (ppt.ppt_name.isClassStaticSynthetic()) // ":::CLASS"
        varname = ppt.ppt_name.getFullClassName() + "." + field;
      else
        throw new Error("Bad ppt: " + ppt);
      varname += "[].class";
      VarInfo vi = ppt.findVar(varname);
      if (vi == null) {
        // This happens, for example, for .class variables
        System.out.println("Warning: MergeESC: Variable not found: " + varname + " at " + ppt);
      } else {
        Assert.assertTrue(vi != null);
        PptSlice1 slice = ppt.findSlice(vi);
        if (slice != null) {
          // System.out.println("Slice for " + vi.name.name());
          {
            EltOneOfString eoos = EltOneOfString.find(slice);
            System.out.println("eoos: " + (eoos == null ? "null" : format((Invariant)eoos)));
            if (eoos != null) {
              String eoos_format = format((Invariant)eoos);
              int et_pos = eoos_format.indexOf(".elementType == \\type(");
              if (et_pos != -1) {
                String type = eoos_format.substring(et_pos + ".elementType == ".length());
                result.put(field, type);
              }
            }
          }
          {
            OneOfStringSequence eooss = OneOfStringSequence.find(slice);
            // System.out.println("eooss: " + (eooss == null ? "null" : format((Invariant)eooss)));
            if (eooss != null) {
              String eooss_format = format((Invariant)eooss);
              int et_pos = eooss_format.indexOf(".elementType == \\type(");
              if (et_pos != -1) {
                String type = eooss_format.substring(et_pos + ".elementType == ".length());
                result.put(field, type);
              }
            }
          }
        }
      }
    }
    return result;
  }

  public String format(Invariant i) {
    if (lightweight)
      return i.format_using(Invariant.OutputFormat.ESCJAVA);
    return i.format_using(Invariant.OutputFormat.JML);
  }
}

// These are the inexpressible invariants; that is, ESC does not support
// them even though JML does.
// sub is_non_supported_invariant( $ ) {
//   my ($inv) = @_;
//   return (($inv =~ /format_esc/)
//           || ($inv =~ /"null"/)
//           || ($inv =~ /\[\] ==/)
//           || ($inv =~ /~/)
//           || ($inv =~ /\bmax\(/)
//           || ($inv =~ /\bmin\(/)
//           || ($inv =~ /\bsum\(/)
//           || ($inv =~ /\\new\(/)
//           || ($inv =~ / has only one value/)
//           || ($inv =~ /\\old\([^\)]*\\old\(/)
//           || ($inv =~ /\\typeof\([^ ]*\.length/));
// }


// ###########################################################################
// ### Main processing
// ###
//
// END {
//
//   for my $javafile (@javafiles) {
//     my @fields = ();                # only non-primitive fields
//     my @owned_fields = ();        # only array fields
//     my @final_fields = ();        # only non-primitive final fields
//     ... set the above fields ...
//
//     my $classname = $javafile;
//     $classname =~ s|\.java$||;  # remove .java
//     $classname =~ s|^\./||;     # in case there is a ./ prefix
//     $classname =~ s|/|.|g;      # all / to .
//
//     open(IN, "$javafile") or die "Cannot open $javafile: $!";
//     open(OUT, ">$javafile-escannotated") or die "Cannot open $javafile-escannotated: $!";
//
//     while (defined($line = <IN>)) {
// ...
//             # Skip @requires clauses for overridden methods which already
//             # have them; ESC doesn't allow them and they perhaps shouldn't hold.
//             my $no_requires = (($ppt =~ /\.equals\s*\(\s*java\.lang\.Object\b/)
//                                || ($ppt =~ /\.toString\s*\(\s*\)/));
//             # add more tests here
//             my $overriding = ($no_requires || 0);
//             # print "overriding=$overriding for $ppt\n";
//             my $requires = ($overriding ? "also_requires" : "requires");
//             my $ensures = ($overriding ? "also_ensures" : "ensures");
//             my $modifies = ($overriding ? "also_modifies" : "modifies");

//             if ($ppt =~ /:::ENTER/) {
//               if (! $no_requires) {
//                 for my $inv (split("\n", $raw{$ppt})) {
//                   if (is_non_supported_invariant($inv)) {
//                     if ($merge_unexpressible) {
//                       print OUT esc_comment("! $requires " . $inv);
//                     }
//                   } else {
//                     print OUT esc_comment("@ $requires " . $inv);
//                   }
//                 }
//               }
//             } elsif ($ppt =~ /:::EXIT/) {
//               my $ppt_combined = $ppt;
//               $ppt_combined =~ s/(:::EXIT)[0-9]+$/$1/;
//               # If this is :::EXIT22 but :::EXIT exists, suppress this.
//               if (($ppt eq $ppt_combined)
//                   || (! exists($raw{$ppt_combined}))) {
//                 for my $inv (split("\n", $raw{$ppt})) {
//                   } elsif ($inv =~ /^The invariant on the following line means:/) {
//                     print OUT esc_comment(" $inv");
//                   } elsif (is_non_supported_invariant($inv)) {
//                     if ($merge_unexpressible) {
//                       print OUT esc_comment("! $ensures " . $inv);
//                     }
//                   } else {
//                     print OUT esc_comment("@ $ensures " . $inv);
//                   }
//                 }
//               } else {
//                 # print OUT "Suppressing $ppt because of $ppt_combined\n";
//               }
//             } else {
//               die "What ppt? $ppt";
//             }
//           }
//         }
//
//         next;
//       }
//
//     }
//


    ///////////////////////////////////////////////////////////////////////////
    /// Old parsing code
    ///

// # Given an arglist string, return a list of arg strings; basically just
// # splits on commas.
// sub args_to_list( $ ) {
//   my ($args) = @_;
//   if (!defined($args)) {
//     confess "undefined args";
//   }
//   $args =~ s/^\s*\(\s*//;
//   $args =~ s/\s*\)\s*$//;
//   $args =~ s/\s+([\[\]])/$1/g;        # remove space before array brackets
//   # remove "final" and such
//   @args = split(/\s*,\s*/, $args);
//   return @args;
// }
//
// # Given an arglist string, return a string with a list of types.
// sub simplify_args( $ ) {
//   my ($args) = @_;
//   my @args = args_to_list($args);
//   my @newargs = ();
//   for my $arg (@args) {
//     # print "before: $arg\n";
//     $arg =~ s/(^|\s)(\w+[\[\]]*)\s+\w+([\[\]]*)$/$1$2/;
//     # print "after: $arg\n";
//     push @newargs, $arg;
//   }
//   $newargs = "(" . join(", ", @newargs) . ")";
//   return $newargs;
// }
//
// ## I'm not sure of the point of the approximate matching.
// ## Maybe string equal would be good enough, if I also used simplify_args.
// # Return true if the arguments are the same modulo whitespace;
// # also, names are permitted to match only up to a prefix.
// sub approx_argsmatch($$) {
//   my ($args1, $args2) = @_;
//   my @args1 = args_to_list($args1);
//   my @args2 = args_to_list($args2);
//
//   # Compare
//   if (scalar(@args1) != scalar(@args2)) {
//     return 0;
//   }
//   for my $i (0..$#args1) {
//     if (! approx_argmatch($args1[$i], $args2[$i])) {
//       return 0;
//     }
//   }
//   return 1;
// }
//
//
// # Return true if the arguments are the same or one is a prefix of the other.
// sub approx_argmatch($$) {
//   my ($x, $y) = @_;
//   if ($x eq $y) {
//     return 1;
//   }
//   if (($x eq "") || ($y eq "")) {
//     return 0;
//   }
//
//   # Ensure $x is not longer than $y.
//   if (length($x) > length($y)) {
//     ($x, $y) = ($y, $x);
//   }
//   if ($x eq substr($y, length($y)-length($x))) {
//     return 1;
//   }
//   return 0;
// }


// # Given a program point name, return the canonical method name
// sub ppt_to_meth( $ ) {
//   my ($ppt) = @_;
//
//   my $methodname = $ppt;
//   # Change "Foo.<init>" to "Foo.Foo".
//   $methodname =~ s/^(\w+)\.<init>\($/$1.$1\(/;
//
//   # Replace arglist by canonicalized version
//   if (($methodname !~ /:::(OBJECT|CLASS)/)
//       && ($methodname !~ s/\(([^\(\)]*)\).*$/&simplify_args($1)/)) {
//     die "Can't parse methodname: $methodname";
//   }
//
//   return $methodname;
// }
//
//
// # Look for the curly brace "{" that begins the method body.
// # Returns a list of ($prebrace, $postbrace, $need_newline).
// sub parse_method_header( $ ) {
//   my ($line) = @_;
//   my ($prebrace, $postbrace, $need_newline);
//
//   # This is because "$)" in regexp screws up Emacs parser.
//   my $eolre = "\\n?\$";
//
//   if ($line =~ /^\s*\{.*$eolre/o) {
//     # Found an open curly brace on this line, following only space.
//     # I'm not sure how this can happen; after all, this line matched a
//     # method declaration.
//     die("How can this happen? line = `$line'");
//
//     $prebrace = "";
//     $postbrace = $line;
//     $need_newline = 0;
//   } elsif ($line =~ /\babstract\b/i) {
//     $prebrace = "";
//     $postbrace = $line;
//     $need_newline = 0;
//   } elsif ($line =~ /^(.*)(\{.*$eolre)/o) {
//     $prebrace = $1;
//     $postbrace = $2;
//     $need_newline = 1;
//   } elsif ($line !~ /\)/) {
//     die "Put all args on same line as declaration:  $line";
//   } else {
//     my $nextline;
//     while (defined($nextline = <IN>)) {
//       if ($nextline =~ m:^\s*(/[/*]|\*):) {
//         # Line starts with "//" or "/*", or "*" which might be comment continuation
//         $line .= $nextline;
//       } elsif ($nextline =~ /^\s*\{.*$eolre/o) {
//         $prebrace = $line;
//         $postbrace = $nextline;
//         $need_newline = 0;
//         last;
//       } elsif ($nextline =~ /^(.*)(\{.*$eolre)/o) {
//         $prebrace = $line . $1;
//         $postbrace = $2;
//         $need_newline = 1;
//         last;
//       } else {
//         die "Didn't find open curly brace in method definition:\n$line\n$nextline";
//       }
//     }
//   }
//   return ($prebrace, $postbrace, $need_newline);
// }


///////////////////////////////////////////////////////////////////////////
/// Other code

//   # maps from method name to canonical program point name
//   my %meth_ppt = ();
//   for my $ppt (keys %raw) {
//     my $methodname = ppt_to_meth($ppt);
//     $meth_ppt{$methodname} = $ppt;
//     # print "method: $methodname\n";
//     # print "ppt: $ppt\n";
//     # print $raw{$ppt};
//   }
