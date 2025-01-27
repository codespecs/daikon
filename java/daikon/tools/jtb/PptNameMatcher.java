package daikon.tools.jtb;

import daikon.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jtb.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import org.checkerframework.checker.nullness.qual.NonNull;
import org.checkerframework.checker.nullness.qual.Nullable;

/**
 * Matches program point names with their corresponding MethodDeclaration's (or
 * ConstructorDeclaration's) in an AST.
 *
 * <p>There are a number of issues in matching, for example, ASTs contain generics, and program
 * point names do not. This implementation handles such issues.
 */
public class PptNameMatcher {

  // Output debugging information when matching a PptName to an AST.
  private static boolean debug_getMatches = false;

  /** Create an AST matcher that will match program points against AST elements rooted at `root'. */
  public PptNameMatcher(Node root) {
    root.accept(new ClassOrInterfaceTypeDecorateVisitor());
  }

  // f0 -> Modifiers()
  // f1 -> [ "final" | Annotation() ]
  // f2 -> Type()
  // f3 -> [ "..." ]
  // f4 -> VariableDeclaratorId()
  public String getUngenerifiedType(FormalParameter p) {

    Type type = p.f2;

    //  Grammar production for type:
    //  f0 -> ReferenceType()
    //        | PrimitiveType()

    if (type.f0.which == 0) {
      // It's a reference type.
      ReferenceType refType = (ReferenceType) type.f0.choice;
      //  Grammar production for ReferenceType:
      //  f0 -> PrimitiveType() ( "[" "]" )+
      //        | ( ClassOrInterfaceType() ) ( "[" "]" )*

      if (refType.f0.which == 0) {
        // It's a primitive array; no generics to handle.
        return Ast.getType(p);

      } else {

        // Make a copy of param (because we may modify it: we may
        // remove some generics stuff).
        // p.accept(new TreeFormatter());
        FormalParameter param = (FormalParameter) Ast.create("FormalParameter", Ast.format(p));

        Type type2 = param.f2;
        ReferenceType refType2 = (ReferenceType) type2.f0.choice;

        // Note the wrapping parentheses in
        //    ( ClassOrInterfaceType() ) ( "[" "]" )*
        NodeSequence intermediateSequence = (NodeSequence) refType2.f0.choice;
        NodeSequence intermediateSequenceOrig = (NodeSequence) refType.f0.choice;
        NodeSequence seq = (NodeSequence) intermediateSequence.elementAt(0);
        NodeSequence seqOrig = (NodeSequence) intermediateSequenceOrig.elementAt(0);

        List<Node> singleElementList = seq.nodes;
        List<Node> singleElementListOrig = seqOrig.nodes;
        // Replace the ClassOrInterfaceType with its ungenerified version.

        //     System.out.println("@0");
        //     param.accept(new TreeDumper());

        // ClassOrInterfaceType t = (ClassOrInterfaceType)singleElementList.get(0);
        ClassOrInterfaceType tOrig = (ClassOrInterfaceType) singleElementListOrig.get(0);
        assert tOrig.unGenerifiedVersionOfThis != null;
        singleElementList.set(0, tOrig.unGenerifiedVersionOfThis);
        // Return getType of the ungenerified version of p.

        // tOrig.unGenerifiedVersionOfThis may have line/col numbering
        // that's inconsistent with param, so we call a formatter
        // here. param is only used for matching, and afterwards it's
        // discarded. So it's ok to reformat it.
        param.accept(new TreeFormatter());

        //     System.out.println("@1");
        //     param.accept(new TreeDumper());
        //     System.out.println("@2");

        return Ast.getType(param);
      }

    } else {
      // It's a primitive; no generics to handle.
      return Ast.getType(p);
    }
  }

  /** Iterates through program points and returns those that match the given method declaration. */
  public List<PptTopLevel> getMatches(PptMap ppts, MethodDeclaration methdecl) {
    return getMatchesInternal(ppts, methdecl);
  }

  /**
   * Iterates through program points and returns those that match the given constructor declaration.
   */
  public List<PptTopLevel> getMatches(PptMap ppts, ConstructorDeclaration constrdecl) {
    return getMatchesInternal(ppts, constrdecl);
  }

  // Iterates through program points and returns those that match the
  // given method or constructor declaration.
  private List<PptTopLevel> getMatchesInternal(PptMap ppts, Node methodOrConstructorDeclaration) {

    List<PptTopLevel> result = new ArrayList<>();

    for (PptTopLevel ppt : ppts.pptIterable()) {
      PptName ppt_name = ppt.ppt_name;

      if (matches(ppt_name, methodOrConstructorDeclaration)) {
        result.add(ppt);
      }
    }

    if (debug_getMatches) {
      System.out.println("getMatchesInternal => " + result);
    }
    return result;
  }

  public boolean matches(PptName pptName, Node methodOrConstructorDeclaration) {

    // This method figures out three things and then calls another
    // method to do the match. The three things are:

    // 1. method name
    // 2. class name
    // 3. method parameters

    String classname;
    String methodname;
    List<FormalParameter> params;

    if (methodOrConstructorDeclaration instanceof MethodDeclaration) {
      classname = Ast.getClassName((MethodDeclaration) methodOrConstructorDeclaration);
      methodname = Ast.getName((MethodDeclaration) methodOrConstructorDeclaration);
      params = Ast.getParameters((MethodDeclaration) methodOrConstructorDeclaration);
    } else if (methodOrConstructorDeclaration instanceof ConstructorDeclaration) {
      classname = Ast.getClassName((ConstructorDeclaration) methodOrConstructorDeclaration);
      methodname = "<init>";
      params = Ast.getParameters((ConstructorDeclaration) methodOrConstructorDeclaration);
    } else {
      throw new Error(
          "Bad type in Ast.getMatches: must be a MethodDeclaration or a ConstructorDeclaration:"
              + methodOrConstructorDeclaration);
    }

    if (debug_getMatches) {
      System.out.printf("getMatches(%s, %s, ...)%n", classname, methodname);
    }
    if (methodname.equals("<init>")) {
      methodname = simpleName(classname);
      if (debug_getMatches) {
        System.out.printf("new methodname: getMatches(%s, %s, ...)%n", classname, methodname);
      }
    }

    if (debug_getMatches) {
      System.out.println("getMatch goal = " + classname + " " + methodname);
    }

    return matches(pptName, classname, methodname, params);
  }

  // True if pptName's name matches the method represented by the rest
  // of the parameters.
  private boolean matches(
      PptName pptName, String classname, String methodname, List<FormalParameter> method_params) {

    // The goal is a fully qualified classname such as
    // samples.calculator.Calculator.AbstractOperandState, but
    // pptName.getFullClassName() can be a binary name such as
    // samples.calculator.Calculator$AbstractOperandState, at least for the
    // :::OBJECT program point.  Is that a bug?

    // Furthermore, pptName.getMethodName may be null for a constructor.

    String pptClassName = pptName.getFullClassName();
    boolean classname_matches =
        (classname.equals(pptClassName)
            || ((pptClassName != null) && classname.equals(pptClassName.replace('$', '.'))));
    String pptMethodName = pptName.getMethodName();
    boolean methodname_matches =
        (methodname.equals(pptMethodName)
            || ((pptMethodName != null)
                && (pptMethodName.indexOf('$') >= 0)
                && methodname.equals(pptMethodName.substring(pptMethodName.lastIndexOf('$') + 1))));

    if (!(classname_matches && methodname_matches)) {
      if (debug_getMatches) {
        System.out.printf(
            "getMatch: class name %s and method name %s DO NOT match candidate.%n",
            pptClassName, pptMethodName);
      }
      return false;
    }
    if (debug_getMatches) {
      System.out.printf(
          "getMatch: class name %s and method name %s DO match candidate.%n",
          classname, methodname);
    }

    List<String> pptTypeStrings = extractPptArgs(pptName);

    if (pptTypeStrings.size() != method_params.size()) {

      // An inner class constructor has an extra first parameter
      // that is an implicit outer this parameter.  If so, remove it
      // before checking for a match.
      boolean OK_outer_this = false;
      if (pptTypeStrings.size() == method_params.size() + 1) {
        String icName = innerConstructorName(pptName);
        if (icName != null) {
          String param0Full = pptTypeStrings.get(0);
          // String param0Simple = simpleName(pptTypeStrings.get(0));
          // Need to check whether param0Simple is the superclass of icName.  How to do that?
          if (classname.startsWith(param0Full + ".")) {
            OK_outer_this = true;
            pptTypeStrings = new ArrayList<String>(pptTypeStrings);
            pptTypeStrings.remove(0);
          }
        }
      }
      if (!OK_outer_this) {
        if (debug_getMatches) {
          System.out.println(
              "arg lengths mismatch: " + pptTypeStrings.size() + ", " + method_params.size());
        }
        return false;
      }
    }

    for (int i = 0; i < pptTypeStrings.size(); i++) {
      String pptTypeString = pptTypeStrings.get(i);
      FormalParameter astType = method_params.get(i);

      if (debug_getMatches) {
        System.out.println(
            "getMatch considering "
                + pptTypeString
                + " ("
                + pptName.getFullClassName()
                + ","
                + pptName.getMethodName()
                + ")");
      }

      if (debug_getMatches) {
        System.out.println("Trying to match at arg position " + Integer.toString(i));
      }

      if (!typeMatch(pptTypeString, astType)) {
        return false;
      } else {
        // continue;
      }
    }

    return true;
  }

  public boolean typeMatch(String pptTypeString, FormalParameter astFormalParameter) {

    if (debug_getMatches) {
      System.out.println(Ast.formatEntireTree(astFormalParameter));
    }

    String astTypeString = getUngenerifiedType(astFormalParameter);

    if (debug_getMatches) {
      System.out.println("Comparing " + pptTypeString + " to " + astTypeString + ":");
    }

    if (Ast.typeMatch(pptTypeString, astTypeString)) {
      if (debug_getMatches) {
        System.out.println("Match arg: " + pptTypeString + " " + astTypeString);
      }
      return true;
    }

    if ((pptTypeString != null) && Ast.typeMatch(pptTypeString, astTypeString)) {
      if (debug_getMatches) {
        System.out.println("Match arg: " + pptTypeString + " " + astTypeString);
      }
      return true;
    }

    if (debug_getMatches) {
      System.out.println("Mismatch arg: " + pptTypeString + " " + astTypeString);
    }

    return false;
  }

  public List<String> extractPptArgs(PptName ppt_name) {

    @SuppressWarnings("nullness") // application invariant
    @NonNull String pptFullMethodName = ppt_name.getSignature();

    if (debug_getMatches) {
      System.out.println("in extractPptArgs: pptFullMethodName = " + pptFullMethodName);
    }
    int lparen = pptFullMethodName.indexOf('(');
    int rparen = pptFullMethodName.indexOf(')');
    assert lparen > 0;
    assert rparen > lparen;
    String ppt_args_string = pptFullMethodName.substring(lparen + 1, rparen);
    String[] ppt_args = ppt_args_string.split(", ");
    if ((ppt_args.length == 1) && ppt_args[0].equals("")) {
      ppt_args = new String[0];
    }

    return Arrays.<String>asList(ppt_args);
  }

  /** Returns simple name of inner class, or null if ppt_name is not an inner constructor. */
  private static @Nullable String innerConstructorName(PptName pptName) {
    String mname = pptName.getMethodName();
    if (mname == null) {
      return null;
    }
    int dollarpos = mname.lastIndexOf('$');
    if (dollarpos >= 0) {
      return mname.substring(dollarpos + 1);
    }
    return null;
  }

  /**
   * Returns the simple name of a possibly-fully-qualified class name. The argument can be a
   * fully-qualified name or a binary name.
   */
  private static String simpleName(String classname) {
    int dotpos = classname.lastIndexOf('.');
    int dollarpos = classname.lastIndexOf('$');
    int pos = Math.max(dotpos, dollarpos);
    if (pos == -1) {
      return classname;
    } else {
      return classname.substring(pos + 1);
    }
  }
}
