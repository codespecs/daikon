// See MergeESC for documentation of behavior.

package daikon.tools.jtb;

import java.util.*;
import java.io.*;
import junit.framework.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import daikon.*;
import utilMDE.ArraysMDE;
import utilMDE.UtilMDE;
import daikon.inv.unary.sequence.EltNonZero;



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

  public PptMap ppts;
  public boolean slashslash;       // whether to use // or /* style comments
  public boolean insert_inexpressible; // whether to insert annotations not supported by ESC

  public Vector addedComments = new Vector(); // elements are NodeTokens

  private String[] ownedFieldNames;  // list of fields in this and related classes
  private String[] finalFieldNames;  // list of fields in this and related classes
  private String[] nonNullElementsFieldNames;  // list of fields in this and related classes


  public MergeESCVisitor(PptMap ppts, boolean slashslash, boolean insert_inexpressible) {
    super();
    this.ppts = ppts;
    this.slashslash = slashslash;
    this.insert_inexpressible = insert_inexpressible;
  }

  void addComment(Node n, String comment, boolean first) {
    NodeToken nt = new NodeToken(comment);
    Ast.addComment(n, nt, first);
    addedComments.add(nt);
  }

  void addComment(Node n, String comment) {
    addComment(n, comment, false);
  }

  void addCommentAfter(Node n, String comment) {
    addComment(Ast.nodeTokenAfter(n), comment, true);
  }

  private boolean isOwned(String fieldname) {
    return (ArraysMDE.indexOf(ownedFieldNames, fieldname) != -1);
  }

  private boolean isFinal(String fieldname) {
    return (ArraysMDE.indexOf(finalFieldNames, fieldname) != -1);
  }

  private boolean isNonNullElements(String fieldname) {
    return (ArraysMDE.indexOf(nonNullElementsFieldNames, fieldname) != -1);
  }

  // ClassDeclaration is a top-level (non-nested) construct.  Collect all
  // the fields in this and any nested class, so that we can recognize
  // assignments to them later on.
  /**
   * f0 -> ( "abstract" | "final" | "public" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
  public void visit(ClassDeclaration n) {
    n.f0.accept(this);
    n.f1.accept(this);
  }

  /**
   * f0 -> ( "static" | "abstract" | "final" | "public" | "protected" | "private" )*
   * f1 -> UnmodifiedClassDeclaration()
   */
  public void visit(NestedClassDeclaration n) {
    n.f0.accept(this);
    n.f1.accept(this);
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
    if (classname.endsWith(".")) {
      classname = classname.substring(0, classname.length()-1);
    }
    String pptname = classname + ":::OBJECT";
    PptTopLevel object_ppt = ppts.get(pptname);

    // Store and restore field names because we must deal with
    // visiting inner classes (which have their own fields)
    String[] old_owned = ownedFieldNames;
    String[] old_final = finalFieldNames;
    String[] old_nonNullElements = nonNullElementsFieldNames;
    { // set fieldNames slots
      CollectFieldsVisitor cfv = new CollectFieldsVisitor();
      n.accept(cfv);
      ownedFieldNames = cfv.ownedFieldNames();
      finalFieldNames = cfv.finalFieldNames();
      nonNullElementsFieldNames = non_null_elements_fields(object_ppt, cfv);
    }

    n.f0.accept(this);
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);

    for (int i=ownedFieldNames.length-1; i>=0; i--) {
      addComment(n.f4.f1, javaLineComment("@ invariant " + ownedFieldNames[i] + ".owner == this"), true);
    }
    if (object_ppt == null) {
      // System.out.println("No object program point found for " + pptname);
    } else {
      String[] obj_invs = Ast.invariants_for(object_ppt, ppts);
      insertInvariants(n.f4.f1, "invariant", obj_invs);
    }

    ownedFieldNames = old_owned;
    finalFieldNames = old_final;
    nonNullElementsFieldNames = old_nonNullElements;
  }

  /**
   * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
   * f1 -> Type()
   * f2 -> VariableDeclarator()
   * f3 -> ( "," VariableDeclarator() )*
   * f4 -> ";"
   */
  public void visit(FieldDeclaration n) {
    n.f0.accept(this);
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);

    if (! Ast.contains(n.f0, "public")) {
      addComment(n, "/*@ spec_public */ ");
    }
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

    n.f0.accept(this);
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);

    Vector matching_ppts = Ast.getMatches(ppts, n);

    String[] requires_invs = null;
    String[] ensures_invs = null;

    for (Iterator itor = matching_ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      String prefix;
      if (ppt.ppt_name.isEnterPoint()) {
        requires_invs = Ast.invariants_for(ppt, ppts);
      } else if (ppt.ppt_name.isExitPoint()) {
	// XXX Change test to whether is unconditional exit, not just any
	/* [INCR]
        if (ppt.combined_exit != null) {
          continue;
        }
	*/
        ensures_invs = Ast.invariants_for(ppt, ppts);
      } else {
        throw new Error("not enter or exit point: " + ppt.name);
      }
    }

    // Special case for main method
    if (Ast.getName(n).equals("main")) {
      List params = Ast.getParameters(n);
      if (params.size() == 1) {
        FormalParameter fp = (FormalParameter)params.get(0);
        String paramtype = Ast.getType(fp);
        if (Ast.typeMatch("java.lang.String[]", paramtype)) {
          if (requires_invs == null) {
            requires_invs = new String[0];
          }
          int old_size = requires_invs.length;
          String[] new_requires_invs = new String[old_size+2];
          System.arraycopy(requires_invs, 0, new_requires_invs, 0, old_size);
          requires_invs = new_requires_invs;
          // should really only add these if they aren't already present
          String param = Ast.getName(fp);
          requires_invs[old_size] = param + " != null";
          requires_invs[old_size+1] = "\\nonnullelements(" + param + ")";
        }
      }
    }

    String ensures_tag = "ensures";
    String requires_tag = "requires";
    String methname = n.f2.f0.tokenImage;
    boolean isOverride = Ast.isOverride(n); // of a superclass
    boolean isImplementation = Ast.isImplementation(n); // of an interface

    if (isImplementation) {
      requires_tag = "also_" + requires_tag;
      ensures_tag = "also_" + ensures_tag;
    }
    if (isOverride) {
      requires_invs = new String[0]; // no requires permitted on overridden methods
      ensures_tag = "also_" + ensures_tag;
    }

    insertInvariants(n, ensures_tag, ensures_invs);
    insertInvariants(n, requires_tag, requires_invs);

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

    n.f0.accept(this);
    n.f1.accept(this);
    n.f2.accept(this);
    n.f3.accept(this);
    n.f4.accept(this);
    n.f5.accept(this);
    n.f6.accept(this);
    n.f7.accept(this);

    Vector matching_ppts = Ast.getMatches(ppts, n);
    // System.out.println("Number of matches (constructor) = " + matching_ppts.size());

    String[] requires = null;
    String[] ensures = null;

    for (Iterator itor = matching_ppts.iterator(); itor.hasNext(); ) {
      PptTopLevel ppt = (PptTopLevel) itor.next();
      String prefix;
      if (ppt.ppt_name.isEnterPoint()) {
        requires = Ast.invariants_for(ppt, ppts);
      } else if (ppt.ppt_name.isExitPoint()) {
	// XXX Change test to whether is unconditional exit, not just any
	/* [INCR]
        if (ppt.combined_exit != null) {
          continue;
        }
	*/
        ensures = Ast.invariants_for(ppt, ppts);
      } else {
        throw new Error("not enter or exit point: " + ppt.name);
      }
    }

    insertInvariants(n, "ensures", ensures);
    insertInvariants(n, "requires", requires);

  }

  public void insertInvariants(Node n, String prefix, String[] invs) {
    if (invs == null) {
      return;
    }
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
		 || (inv.endsWith(".toString"))) {
        // inexpressible invariant; need to check flag and output it
        continue;
      } else if (inv.startsWith("modifies ")) {
        if (prefix.startsWith("also_")) {
          inv = "also_" + inv;
        }
        addComment(n, javaLineComment("@ " + inv), true);
      } else {
        addComment(n, javaLineComment("@ " + prefix + " " + inv), true);
      }
    }
  }

  /**
   * f0 -> PreIncrementExpression()
   *       | PreDecrementExpression()
   *       | PrimaryExpression() [ "++" | "--" | AssignmentOperator() Expression() ]
   */
  public void visit(StatementExpression n) {
    n.f0.accept(this);

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
          // it's an assignment
          String fieldname = fieldName(pe);
          // System.out.println("In statement, fieldname = " + fieldname);
          if ((fieldname != null)
              && (isOwned(fieldname) || isNonNullElements(fieldname))) {
            ConstructorDeclaration cd
              = (ConstructorDeclaration) Ast.getParent(ConstructorDeclaration.class, n);
            MethodDeclaration md
              = (MethodDeclaration) Ast.getParent(MethodDeclaration.class, n);
            if ((cd != null)
                || ((md != null) && (! Ast.contains(md.f0, "static")))) {
              Node parent = Ast.getParent(Statement.class, n);
              // This may be wrong if parent isn't in a block (eg, if parent
              // is sole element in then or else clause).
              if (isOwned(fieldname)) {
                addCommentAfter(parent, javaLineComment("@ set " + fieldname + ".owner = this"));
              }
              if (isNonNullElements(fieldname)) {
                addCommentAfter(parent, javaLineComment("@ set " + fieldname + ".containsNull = false"));
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
    n.f0.accept(this);
    n.f1.accept(this);

    if (n.f1.present()) {
      // it's an assignment
      // System.out.println("found an assignment expression: " + n);
      PrimaryExpression pe = assignment2primaryexpression(n);
      String fieldname = fieldName(pe);
      // System.out.println("In expression, fieldname = " + fieldname);
      Node stmt = Ast.getParent(Statement.class, n);
      if ((fieldname != null) && isOwned(fieldname)) {
        addComment(stmt, javaLineComment("@ set " + fieldname + ".owner = this"));
      }

    }

  }

  // Return the primary expression on the left-hand side of an assignment
  private PrimaryExpression assignment2primaryexpression(Expression n) {
    Assert.assert(n.f1.present());
    ConditionalExpression ce = n.f0;
    Assert.assert(! ce.f1.present());
    ConditionalOrExpression coe = ce.f0;
    Assert.assert(! coe.f1.present());
    ConditionalAndExpression cae = coe.f0;
    Assert.assert(! cae.f1.present());
    InclusiveOrExpression ioe = cae.f0;
    Assert.assert(! ioe.f1.present());
    ExclusiveOrExpression eoe = ioe.f0;
    Assert.assert(! eoe.f1.present());
    AndExpression ande = eoe.f0;
    Assert.assert(! ande.f1.present());
    EqualityExpression ee = ande.f0;
    Assert.assert(! ee.f1.present());
    InstanceOfExpression iofe = ee.f0;
    Assert.assert(! iofe.f1.present());
    RelationalExpression re = iofe.f0;
    Assert.assert(! re.f1.present());
    ShiftExpression se = re.f0;
    Assert.assert(! se.f1.present());
    AdditiveExpression adde = se.f0;
    Assert.assert(! adde.f1.present());
    MultiplicativeExpression me = adde.f0;
    Assert.assert(! me.f1.present());
    UnaryExpression ue = me.f0;
    UnaryExpressionNotPlusMinus uenpm
      = (UnaryExpressionNotPlusMinus) ue.f0.choice;
    PostfixExpression pfe = (PostfixExpression) uenpm.f0.choice;
    Assert.assert(! pfe.f1.present());
    PrimaryExpression pe = pfe.f0;
    return pe;
  }


  private String fieldName(PrimaryExpression pe) {
    // System.out.println("fieldName(" + pe + ")");

    // Get a name from the PrimaryPrefix, if possible.
    String result = null;
    PrimaryPrefix pp = pe.f0;
    NodeChoice ppnc = pp.f0;
    // PrimaryPrefix:
    /**
     * f0 -> Literal()
     *       | "this"
     *       | "super" "." <IDENTIFIER>
     *       | "(" Expression() ")"
     *       | AllocationExpression()
     *       | ResultType() "." "class"
     *       | Name()
     */
    switch (ppnc.which) {
    case 2:
      NodeSequence sn = (NodeSequence) ppnc.choice;
      Assert.assert(sn.size() == 3);
      result = ((NodeToken) sn.elementAt(2)).tokenImage;
      break;
    case 6:
      result = fieldName((Name) ppnc.choice);
      break;
    }

    // Get a name from the PrimarySuffix, if possible.
    NodeListOptional pslist = pe.f1;
    for (int i=0; i<pslist.size(); i++) {
      PrimarySuffix ps = (PrimarySuffix) pslist.elementAt(i);
      NodeChoice psnc = ps.f0;
      // PrimarySuffix:
      /**
       * f0 -> "." "this"
       *       | "." AllocationExpression()
       *       | "[" Expression() "]"
       *       | "." <IDENTIFIER>
       *       | Arguments()
       */
      switch (psnc.which) {
      case 4:
        NodeSequence sn = (NodeSequence) ppnc.choice;
        Assert.assert(sn.size() == 2);
        result = ((NodeToken) sn.elementAt(1)).tokenImage;
        break;
      default:
        // in all other cases, reset to null
        result = null;
        break;
      }
    }

    return result;
  }

  private String fieldName(Name n) {
    NodeListOptional nlo = n.f1;
    if (nlo.present()) {
      NodeSequence ns = (NodeSequence) nlo.elementAt(nlo.size()-1);
      Assert.assert(ns.size() == 2);
      NodeToken nt = (NodeToken) ns.elementAt(1);
      return nt.tokenImage;
    } else {
      return n.f0.tokenImage;
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


// ###########################################################################
// ### Subroutines
// ###

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


  // ppt is an :::OBJECT program point
  String[] non_null_elements_fields(PptTopLevel ppt, CollectFieldsVisitor cfv) {
    Vector result = new Vector();
    String[] fields = cfv.allFieldNames();
    for (int i=0; i<fields.length; i++) {
      String field = fields[i];
      VarInfo vi = ppt.findVar("this." + field);
      Assert.assert(vi != null);
      PptSlice1 slice = ppt.getView(vi);
      if (slice != null) {
        EltNonZero enz = EltNonZero.find(slice);
        if (enz != null) {
          result.add(field);
        }
      }
    }
    return (String[]) result.toArray(new String[0]);
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
