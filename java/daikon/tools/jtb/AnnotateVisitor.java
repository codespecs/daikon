// See Annotate for documentation of behavior.

package daikon.tools.jtb;

import java.io.*;
import java.util.*;
import java.util.regex.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.*;
import utilMDE.Assert;
import utilMDE.ArraysMDE;
import daikon.inv.Invariant;
import daikon.inv.OutputFormat;
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

public class AnnotateVisitor extends DepthFirstVisitor {

  private static final String lineSep = System.getProperty("line.separator");

  public static final String JML_START_COMMENT = "/*@" + lineSep;
  public static final String JML_END_COMMENT = "@*/" + lineSep;

  public List<String> javaFileLines;

  public PptMap ppts;
  public boolean slashslash;       // whether to use // or /* style comments
  public boolean insert_inexpressible; // whether to insert annotations not supported by ESC
  public boolean lightweight;      // whether to use full JML specs or lightweight ESC specs instead

  // whether to use reflection when trying to figure out if a method
  // overrides/implements another method. If this variable is set to
  // false, then Annotate will not try to determine if a method
  // overrides/implements another method, which means that it will not
  // try to add "also" tags to its output.
  public boolean useReflection;

  // If the --max_invariants_pp option is given, this variable is set
  // to the maximum number of invariants out annotate per program point.
  public int maxInvariantsPP;


  public Vector<NodeToken> addedComments = new Vector<NodeToken>();

  private String[] ownedFieldNames;  // list of fields in this and related classes
  private String[] finalFieldNames;  // list of fields in this and related classes
  private String[] notContainsNullFieldNames;  // list of fields in this and related classes
  private HashMap<String,String> elementTypeFieldNames; // list of fields in this and related classes

  private PptNameMatcher pptMatcher;

  public AnnotateVisitor(String javafilename,
                         Node root,
                         PptMap ppts,
                         boolean slashslash,
                         boolean insert_inexpressible,
                         boolean lightweight,
                         boolean useReflection,
                         int maxInvariantsPP) {

    initialize(javafilename,
               root,
               ppts,
               slashslash,
               insert_inexpressible,
               lightweight,
               useReflection,
               maxInvariantsPP);

  }

  private void initialize(String javafilename,
                          Node root,
                          PptMap ppts,
                          boolean slashslash,
                          boolean insert_inexpressible,
                          boolean lightweight,
                          boolean useReflection,
                          int maxInvariantsPP) {

    this.pptMatcher = new PptNameMatcher(root);

    // Read in the java file into a list of Strings.
    this.javaFileLines = new ArrayList<String>();
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(javafilename));
    } catch (FileNotFoundException e) {
      throw new Error(e);
    }
    String line = null;
    try {
      line = reader.readLine();
    } catch (IOException e) {
      try {
        reader.close();
      } catch (IOException e2) {
        // ignore second exception
      }
      throw new Error(e);
    }
    while (line != null) {
      this.javaFileLines.add(line);
      try {
        line = reader.readLine();
      } catch (IOException e) {
        try {
          reader.close();
        } catch (IOException e2) {
          // ignore second exception
        }
        throw new Error(e);
      }
    }
    try {
      reader.close();
    } catch (IOException e) {
      throw new Error(e);
    }

    this.ppts = ppts;
    this.slashslash = slashslash;
    this.insert_inexpressible = insert_inexpressible;
    this.lightweight = lightweight;
    this.useReflection = useReflection;
    this.maxInvariantsPP = maxInvariantsPP;
  }

  // Like Ast.addComment, but also keeps a list of what comments were added.
  void addComment(Node n, String comment, boolean first) {
    NodeToken nt = new NodeToken(comment);
    Ast.findLineAndCol(n, nt, first);
    addedComments.add(nt);
    //System.out.println("comment.beginLine:" + nt.beginLine);
    //System.out.println("comment.beginColumn:" + nt.beginColumn);
    int line = nt.beginLine - 1; /* because in jtb lines start at 1 */
    String lineString = (String)javaFileLines.get(line);
    int column = getTabbedIndex(nt.beginColumn - 1, /* because in jtb cols start at 1 */
                                lineString);
    StringBuffer sb = new StringBuffer();
    sb.append(lineString.substring(0, column));
    sb.append(comment);
    if (comment.endsWith("\n")) {
      // Insert the whitespace precedgin the line
      sb.append(precedingWhitespace(lineString.substring(0,column)));
    }
    sb.append(lineString.substring(column));
    javaFileLines.set(line, sb.toString());
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
    return elementTypeFieldNames.get(fieldname);
  }

  // ClassDeclaration is a top-level (non-nested) construct.  Collect all
  // the fields in this and any nested class, so that we can recognize
  // assignments to them later on.
  /**
   * f0 -> ( "abstract" | "final" | "public" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
//   public void visit(ClassOrInterfaceDeclaration n) {
//     super.visit(n);             // call "accept(this)" on each field
//   }

  /**
   * f0 -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
//   public void visit(NestedClassDeclaration n) {
//     super.visit(n);             // call "accept(this)" on each field
//   }

  // Insert object invariants for this class.
  // Insert owner assertions for fields.
/**
 * Grammar production:
 * f0 -> ( "class" | "interface" )
 * f1 -> <IDENTIFIER>
 * f2 -> [ TypeParameters() ]
 * f3 -> [ ExtendsList(isInterface) ]
 * f4 -> [ ImplementsList(isInterface) ]
 * f5 -> ClassOrInterfaceBody(isInterface)
 */
  public void visit(ClassOrInterfaceDeclaration n) {
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
    HashMap<String,String> old_elementType = elementTypeFieldNames;
    { // set fieldNames slots
      CollectFieldsVisitor cfv = new CollectFieldsVisitor();
      n.accept(cfv);
      ownedFieldNames = cfv.ownedFieldNames();
      finalFieldNames = cfv.finalFieldNames();
      if (object_ppt == null) {
        notContainsNullFieldNames = new String[0];
        elementTypeFieldNames = new HashMap<String,String>();
      } else {
        notContainsNullFieldNames = not_contains_null_fields(object_ppt, cfv);
        elementTypeFieldNames = element_type_fields(object_ppt, cfv);
      }
    }

    super.visit(n);             // call "accept(this)" on each field

    if (lightweight && (Daikon.output_format != OutputFormat.DBCJAVA)) {
      for (int i=ownedFieldNames.length-1; i>=0; i--) {
        addComment(n.f5.f1, javaLineComment("@ invariant" + " " + ownedFieldNames[i] + ".owner == this;"), true);
      }
    }
    if (object_ppt == null) {
      // System.out.println("No object program point found for " + classname);
    } else {
      InvariantsAndModifiedVars obj_invs = invariants_for(object_ppt, ppts);
      String inv_tag = (Daikon.output_format == OutputFormat.DBCJAVA ? "@invariant" : "invariant");
      insertInvariants(n.f5.f1, inv_tag, obj_invs);
    }

    ownedFieldNames = old_owned;
    finalFieldNames = old_final;
    notContainsNullFieldNames = old_notContainsNull;
    elementTypeFieldNames = old_elementType;
  }

  public void visit(FieldDeclaration n) {
    super.visit(n);             // call "accept(this)" on each field

    // Nothing to do for Jtest DBCJAVA format
    if (Daikon.output_format == OutputFormat.DBCJAVA) { return; }

    if (! Ast.contains(n.f0, "public")) {
//       n.accept(new TreeFormatter());
//       String nString = Ast.print(n);
//       System.out.println("@@@");
//       Node n2 = n.getParent().getParent(); n2.accept(new TreeFormatter());
//       System.out.println("@@@" + Ast.print(n2));
      addComment(n.getParent().getParent(), "/*@ spec_public */ ");
    }
  }

  // Node n is a MethodDeclaration or a ConstructorDeclaration
  InvariantsAndModifiedVars[] get_requires_and_ensures(PptMap ppts, Node n) {
    InvariantsAndModifiedVars requires_invs = null;
    InvariantsAndModifiedVars ensures_invs = null;

    List<PptTopLevel> matching_ppts = null;
    if (n instanceof MethodDeclaration) {
      matching_ppts = pptMatcher.getMatches(ppts, (MethodDeclaration)n);
    } else if (n instanceof ConstructorDeclaration) {
      matching_ppts = pptMatcher.getMatches(ppts, (ConstructorDeclaration)n);
    } else {
      throw new Error("Node must be MethodDeclaration or ConstructorDeclaration");
    }

    for (Iterator<PptTopLevel> itor = matching_ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel ppt = itor.next();
      String prefix;
      if (ppt.ppt_name.isEnterPoint()) {
        requires_invs = invariants_for(ppt, ppts);
      } else if (ppt.ppt_name.isExitPoint()) {
        if (! ppt.ppt_name.isCombinedExitPoint()) {
          continue;
        }
        ensures_invs = invariants_for(ppt, ppts);
      }
    }

    return new InvariantsAndModifiedVars[] { requires_invs, ensures_invs };
  }


  // Does not appear to be used (and doesn't typecheck, anyway). -MDE 7/12/2005
  // HashMap<String,Collection<Invariant>> get_exceptions(PptMap ppts, ConstructorDeclaration n) {
  //   HashMap<String,Collection<Invariant>> result = new HashMap<String,Collection<Invariant>>();
  //
  //   List<PptTopLevel> matching_ppts = pptMatcher.getMatches(ppts, n);
  //
  //   for (Iterator<PptTopLevel> itor = matching_ppts.iterator(); itor.hasNext(); ) {
  //     PptTopLevel ppt = itor.next();
  //     String prefix;
  //     if (ppt.ppt_name.isThrowsPoint()) {
  //       String exceptionName = "Not getting called"; // ppt.ppt_name.dontKnowHowToDoThis();
  //       Collection<Invariant> exceptionInvariants;
  //
  //       if (result.containsValue(exceptionName)) {
  //         exceptionInvariants = result.get(exceptionName);
  //         exceptionInvariants.add(ppt.getInvariants());
  //       } else {
  //         exceptionInvariants = new Vector<Invariant>(ppt.getInvariants());
  //         result.put(exceptionName, exceptionInvariants);
  //       }
  //     }
  //   }
  //
  //   return result;
  // }

  public void insertAlso(Node n) {
    addComment(n, "@ also" + lineSep, true);
  }

  // n must be a MethodDeclaration or ConstructorDeclaration
  public void insertBehavior(Node n) {
    class InsertBehaviorVisitor extends DepthFirstVisitor {
      Node n;
      boolean behaviorInserted;

      public InsertBehaviorVisitor(Node n) {
        super();
        this.n = n;
        behaviorInserted = false;
      }

      private String getBehaviorString() {
        return "normal_behavior";
      }

      public void visit(NodeChoice nc) {
        //System.out.println("InsertBehavior visitor visiting a NodeChoice");
         String modifier = (nc != null && nc.choice != null ? nc.choice.toString() : "");
         //System.out.println("A node choice here: " + modifier);
//          StringWriter sb = new StringWriter();
//          TreeDumper d = new TreeDumper(sb);

//          if (n instanceof MethodDeclaration)
//            d.visit((MethodDeclaration)n);
//          else
//            d.visit((ConstructorDeclaration)n);

//          System.out.println("Tree dump of n: " + sb.toString());
        if (Ast.isAccessModifier(modifier)) {
          addComment(n.getParent().getParent(), "@ " + modifier + " " + getBehaviorString() +
                     (Daikon.output_format == OutputFormat.JML
                      ? " // Generated by Daikon"
                      : "") +
                     lineSep, true);
          behaviorInserted = true;
        }
      }

      public void visit(NodeOptional no) {
        //System.out.println("InsertBehavior visitor visiting a NodeOptional");
        visit((NodeChoice)no.node);
      }

      public void visit(NodeListOptional nlo) {
        //System.out.println("InsertBehavior visitor visiting a NodeListOptional");
        //System.out.println("With " + nlo.nodes.size() + " nodes");
        for (int i=0; i<nlo.nodes.size() && !behaviorInserted; i++) {
          //System.out.println("Visiting a NodeChoice");

          // The other way around (that is, ((NodeChoice)nlo...).accept(this))
          // does not work because of the way NodeChoice.accept is defined
          visit((NodeChoice)nlo.nodes.get(i));
        }
        if (!behaviorInserted)
          addComment(n.getParent().getParent(), "@ " + "private " + getBehaviorString() +
                     (Daikon.output_format == OutputFormat.JML
                      ? " // Generated by Daikon"
                      : "") +
                     lineSep, true);
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

    InsertBehaviorVisitor v = new InsertBehaviorVisitor(n);
    if (n instanceof MethodDeclaration)
      ((MethodDeclaration)n).getParent().getParent().accept(v);
    else
      ((ConstructorDeclaration)n).getParent().getParent().accept(v);
  }


  public void visit(MethodDeclaration n) {

/**
 * Grammar production for ClassOrInterfaceBodyDeclaration:
 * f0 -> Initializer()
 *       | Modifiers() ( ClassOrInterfaceDeclaration(modifiers) | EnumDeclaration(modifiers) | ConstructorDeclaration() | FieldDeclaration(modifiers) | MethodDeclaration(modifiers) )
 *       | ";"
 */


    super.visit(n);             // call "accept(this)" on each field


    InvariantsAndModifiedVars[] requires_and_ensures = get_requires_and_ensures(ppts, n);

    InvariantsAndModifiedVars requires_invs = requires_and_ensures[0];
    InvariantsAndModifiedVars ensures_invs = requires_and_ensures[1];

    String ensures_tag = (Daikon.output_format == OutputFormat.DBCJAVA ? "@post" : "ensures");
    String requires_tag = (Daikon.output_format == OutputFormat.DBCJAVA ? "@pre" : "requires");

    boolean isOverride = false;
    boolean isImplementation = false;
    if (useReflection) {
      isOverride = Ast.isOverride(n); // of a superclass
      isImplementation = Ast.isImplementation(n); // of an interface
    }

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
      addComment(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, JML_END_COMMENT, true);

    boolean invariantInserted =
      insertInvariants(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, ensures_tag, ensures_invs, lightweight);

          if (ensures_invs != null) {
            insertModifies(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, ensures_invs.modifiedVars, ensures_tag, lightweight);
          }

    invariantInserted =
      insertInvariants(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, requires_tag, requires_invs, lightweight) ||
      invariantInserted;

    if (!lightweight) {
      if (!invariantInserted) {
        insertJMLWorkaround(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */);
      }
      insertBehavior(n);
      if (isImplementation || isOverride
          // temporary fix: not processed correctly by Ast.java
          || n.f2.f0.toString().equals("clone")) {
        insertAlso(n.getParent().getParent());
      }
      addComment(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, JML_START_COMMENT, true);
    }
  }

/**
 * Grammar production:
 * f0 -> [ TypeParameters() ]
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

    InvariantsAndModifiedVars[] requires_and_ensures = get_requires_and_ensures(ppts, n);
    InvariantsAndModifiedVars requires_invs = requires_and_ensures[0];
    InvariantsAndModifiedVars ensures_invs = requires_and_ensures[1];

    String ensures_tag = (Daikon.output_format == OutputFormat.DBCJAVA ? "@post" : "ensures");
    String requires_tag = (Daikon.output_format == OutputFormat.DBCJAVA ? "@pre" : "requires");

    if (!lightweight) {
      addComment(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, JML_END_COMMENT, true);
    }

    boolean invariantInserted =
      insertInvariants(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */,
                       ensures_tag, ensures_invs, lightweight);

    if (ensures_invs != null) {
      insertModifies(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */,
                     ensures_invs.modifiedVars, ensures_tag, lightweight);
    }

    invariantInserted =
      insertInvariants(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */,
                       requires_tag, requires_invs, lightweight) ||
      invariantInserted;

    if (!lightweight) {
      if (!invariantInserted) {
        insertJMLWorkaround(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */);
      }
      insertBehavior(n);
      addComment(n.getParent().getParent() /* see  ClassOrInterfaceBodyDeclaration */, JML_START_COMMENT, true);
    }

  }


  public boolean pureInJML(Node n) {
    String name = null;
    if (n instanceof MethodDeclaration) {
      name = ((MethodDeclaration)n).f2.f0.toString();
      // have to do this because ... ?
      if (name.equals("hasNext") ||
          name.equals("hasMoreElements") ||
          name.equals("equals") ||
          name.equals("clone")) {
        return true;
      }
    }
    return false;
  }

  public void insertJMLWorkaround(Node n) {
    addComment(n, "@ requires true;" + lineSep, true);
  }

  public boolean insertInvariants(Node n, String prefix, InvariantsAndModifiedVars invs) {
    return insertInvariants(n, prefix, invs, true);
  }

  public boolean insertModifies(Node n, String modifiesString, String prefix, boolean useJavaComment) {

    modifiesString = modifiesString.trim();

    if (modifiesString.equals("modifies") || modifiesString.equals("assignable")) {
      // No variables found.
      return false;
    }

    if (!(modifiesString.startsWith("modifies") || modifiesString.startsWith("assignable"))) {
      // Doesn't look ilke a modifies clause.
      return false;
    }

    String inv = null;
    boolean doInsert = true;

    if (Daikon.output_format == OutputFormat.JML) {
      if (pureInJML(n)) {
        doInsert = false;
      } else {
        //System.out.println("^^^CURRLINE:" + Ast.printCurrentLine(n) + "^^^");
        inv = "assignable \\everything";
      }
    } else if (Daikon.output_format == OutputFormat.DBCJAVA) {
      // Modifies/assignable has no translation in Jtest DBC
      doInsert = false;
    } else if (lightweight && prefix.startsWith("also_")) {
      inv = "also_" + modifiesString;
    } else {
      inv = modifiesString;
    }

    if (!doInsert) {
      return false;
    }

    String commentContents = "@ " + inv + ";";
    if (useJavaComment) {
      commentContents = javaLineComment(commentContents);
    } else {
      commentContents += lineSep;
    }
    addComment(n, commentContents, true);
    return true;
  }

  // The "invs" argument may be null, in which case no work is done.
  public boolean insertInvariants(Node n, String prefix, InvariantsAndModifiedVars invs,
                                  boolean useJavaComment) {
    if (invs == null) {
      return false;
    }

    boolean invariantInserted = false;
    boolean assignableInserted = false;

    int maxIndex = invs.invariants.size();
    if (maxInvariantsPP > 0 && maxInvariantsPP < maxIndex) {
      maxIndex = maxInvariantsPP;
    }

    for (int i = maxIndex-1 ; i >= 0 ; i--) {
      Invariant inv = (Invariant)invs.invariants.get(i);
      if (! inv.isValidExpression(Daikon.output_format)) {
        // inexpressible invariant
        if (insert_inexpressible) {
          addComment(n, javaLineComment("! " + inv + ";"), true);
        }
        continue;
      } else {
        String commentContents = (Daikon.output_format == OutputFormat.DBCJAVA ? "  " : "@ ")
          + prefix + " " + inv.format_using(Daikon.output_format)
          + (Daikon.output_format == OutputFormat.DBCJAVA ? "  " : ";");
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

    // Nothing to do for Jtest DBC format
    if (Daikon.output_format == OutputFormat.DBCJAVA) { return; }

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

          // System.out.printf("In statement, fieldname = %s", fieldname);
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
    Vector<String> result = new Vector<String>();
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
        // This happens, for example, for static final vars (see
        // REP_SCALE_FACTOR in MapQuick1/GeoPoint.java).
        System.out.println("Warning: Annotate: skipping Variable " + varname + " at " + ppt);
      } else {
        // vi != null
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
  HashMap<String,String> element_type_fields(PptTopLevel ppt, CollectFieldsVisitor cfv) {
    // System.out.println("not_contains_null_fields(" + ppt + ")");
    HashMap<String,String> result = new HashMap<String,String>();
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
        // This means that we found a variable in the source code that is
        // not computed by Daikon.
        System.out.println("Warning: Annotate: Daikon knows nothing about variable " + varname + " at " + ppt);
      } else {
        // vi != null
        PptSlice1 slice = ppt.findSlice(vi);
        if (slice != null) {
          // System.out.println("Slice for " + vi.name.name());
          {
            EltOneOfString eoos = EltOneOfString.find(slice);
            // System.out.println("eoos: " + (eoos == null ? "null" : format((Invariant)eoos)));
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

  public String format(Invariant inv) {
    String inv_string;
    if (lightweight) {
      inv_string = inv.format_using(OutputFormat.ESCJAVA);
    } else {
      inv_string = inv.format_using(OutputFormat.JML);
    }
    // Debugging
    // if (true) {
    //   inv_string = inv_string + "  REPR: " + inv.repr();
    // }
    return inv_string;
  }

  public static InvariantsAndModifiedVars invariants_for(PptTopLevel ppt,
                                                         PptMap pptmap) {

    StringWriter sw = new StringWriter();
    PrintWriter pw = new PrintWriter(sw);
    PrintInvariants.print_modified_vars(ppt, pw);

    InvariantsAndModifiedVars retval = new InvariantsAndModifiedVars();
    retval.invariants = Ast.getInvariants(ppt, pptmap);
    retval.modifiedVars = sw.toString();

    // PrintInvariants.print_modified_vars(ppt, pw) returns possibly
    // several lines. In such a case, we're only interested in the second
    // one, which contains the "modified" or "assignable" clause.
    String[] splitModVars = retval.modifiedVars.split(lineSep);
    if (splitModVars.length > 1) {
      for (int i = 0 ; i < splitModVars.length ; i++) {
        if (splitModVars[i].startsWith("modifies ") ||
            splitModVars[i].startsWith("assignable ")) {
          retval.modifiedVars =  splitModVars[i];
          break;
        }
      }
    }

    return retval;
  }

  private static class InvariantsAndModifiedVars {
    public List<Invariant> invariants;
    public String modifiedVars;
  }

  // Consider a line L1 of text that contains some tabs.
  //
  // untabbedIndex represents an index into a line L2, where L2 is L1
  // with all tabs substituted by their corresponding number of blank
  // spaces, where a tab's width is 8 characters.
  //
  // This method returns the index into L1 that corresponds to the same
  // position in L2.
  //
  // I assume that untabbedIndex will not be an index into one of the
  // blank spaces that replaced some tab.
  public static int getTabbedIndex(int untabbedIndex, String L1) {
    if (untabbedIndex == 0) {
      return 0;
    }
    int expanded = 0;
    int index = 0;
    for (int i = 0 ; i < L1.length() ; i++) {

      if (expanded > untabbedIndex) {
        throw new RuntimeException("expanded:" + expanded
                                   + "untabbedIndex:" + untabbedIndex
                                   + "\nL1:" + L1);
      } else if (expanded == untabbedIndex) {
        index = i;
        break;
      }

      char c = L1.charAt(i);
      if (c == '\t') {
        expanded += 8 - (expanded % 8);
      } else {
        expanded += 1;
      }

    }

    Assert.assertTrue(expanded == untabbedIndex,
                      "\nexpanded:" + expanded
                      + "\nuntabbedIndex:" + untabbedIndex
                      + "\nL1: " + L1);
    return index;

  }

  public static String precedingWhitespace(String s) {
    for (int i = 0 ; i < s.length() ; i++) {
      if (!Character.isWhitespace(s.charAt(i))) {
        return s.substring(0, i);
      }
    }
    return s;
  }

}
