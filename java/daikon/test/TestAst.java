package daikon.test;

import static org.junit.Assert.assertEquals;

import daikon.PptName;
import daikon.tools.jtb.*;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.List;
import jtb.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import junit.framework.*;
import org.junit.Test;

/**
 * Tests functionality of some methods in daikon.tools.jtb.Ast.
 *
 * <p>TODO implement and test handling of "..." construct.
 */
public final class TestAst {

  public static class MethodDeclarationHarvester extends DepthFirstVisitor {
    List<MethodDeclaration> decls = new ArrayList<>();

    @Override
    public void visit(MethodDeclaration m) {

      decls.add(m);

      m.f0.accept(this);
      m.f1.accept(this);
      m.f2.accept(this);
      m.f3.accept(this);
      m.f4.accept(this);
    }
  }

  public static class ClassOrInterfaceDeclarationHarvester extends DepthFirstVisitor {
    List<ClassOrInterfaceDeclaration> decls = new ArrayList<>();

    @Override
    public void visit(ClassOrInterfaceDeclaration m) {
      decls.add(m);
      m.f0.accept(this);
      m.f1.accept(this);
      m.f2.accept(this);
      m.f3.accept(this);
      m.f4.accept(this);
      m.f5.accept(this);
    }
  }

  private void checkMatch(String pptNameString, MethodDeclaration decl, PptNameMatcher matcher) {

    PptName pptName = new PptName(pptNameString);
    boolean result = matcher.matches(pptName, decl);
    if (result == false) {
      // Format so we can print an error message.
      decl.accept(new TreeFormatter());
      String declString = Ast.format(decl);
      throw new Error(
          String.join(
              System.lineSeparator(),
              "pptName: " + pptName,
              "doesn't match method declaration:",
              "----------",
              declString,
              "----------"));
    }
  }

  @Test
  public void test_Ast_Ppt_Match() {

    CompilationUnit compilationUnit;

    // Parse the file "GenericTestClass.java" (under same dir as this class)
    try (InputStream sourceIn = this.getClass().getResourceAsStream("GenericTestClass.java")) {
      if (sourceIn == null) {
        throw new Error("Couldn't find file GenericTestClass.java");
      }
      JavaParser parser = new JavaParser(sourceIn);

      try {
        compilationUnit = parser.CompilationUnit();
      } catch (ParseException e) {
        throw new Error(e);
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }

    // Test class declarations

    // Pick off class declarations
    ClassOrInterfaceDeclarationHarvester classDeclarationHarvester =
        new ClassOrInterfaceDeclarationHarvester();

    compilationUnit.accept(classDeclarationHarvester);
    List<ClassOrInterfaceDeclaration> classDecls = classDeclarationHarvester.decls;

    {
      String expected = "daikon.test.GenericTestClass";
      assertEquals(expected, Ast.getClassName(classDecls.get(0)));
    }
    // Illegal in Java 6
    // {
    //   assertEquals("daikon.test.GenericTestClass.Simple", Ast.getClassName(classDecls.get(1)));
    // }

    // Test method declarations

    // Pick off method declarations
    MethodDeclarationHarvester methodDeclarationHarvester = new MethodDeclarationHarvester();

    compilationUnit.accept(methodDeclarationHarvester);

    List<MethodDeclaration> methodDecls = methodDeclarationHarvester.decls;

    PptNameMatcher matcher = new PptNameMatcher(compilationUnit);

    MethodDeclaration decl;

    decl = methodDecls.get(0);
    assertEquals("foo1", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo1():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo1():::EXIT10", decl, matcher);

    decl = methodDecls.get(1);
    assertEquals("foo2", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo2():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo2():::EXIT12", decl, matcher);

    decl = methodDecls.get(2);
    assertEquals("foo3", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo3():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo3():::EXIT14", decl, matcher);

    decl = methodDecls.get(3);
    assertEquals("foo4", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo4():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo4():::EXIT16", decl, matcher);

    decl = methodDecls.get(4);
    assertEquals("foo5", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo5():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo5():::EXIT18", decl, matcher);

    decl = methodDecls.get(5);
    assertEquals("foo55", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo55():::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo55():::EXIT20", decl, matcher);

    decl = methodDecls.get(6);
    assertEquals("foo6", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo6(java.util.List):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo6(java.util.List):::EXIT22", decl, matcher);

    decl = methodDecls.get(7);
    assertEquals("foo7", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo7(java.util.List):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo7(java.util.List):::EXIT24", decl, matcher);

    decl = methodDecls.get(8);
    assertEquals("foo8", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo8(java.lang.Object):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo8(java.lang.Object):::EXIT26", decl, matcher);

    decl = methodDecls.get(9);
    assertEquals("foo9", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo9(java.lang.String):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo9(java.lang.String):::EXIT28", decl, matcher);

    decl = methodDecls.get(10);
    assertEquals("foo10", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo10(java.lang.Object):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo10(java.lang.Object):::EXIT30", decl, matcher);

    decl = methodDecls.get(11);
    assertEquals("foo11", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo11(java.lang.Comparable, java.lang.Object):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo11(java.lang.Comparable, java.lang.Object):::EXIT32",
        decl,
        matcher);

    decl = methodDecls.get(12);
    assertEquals("foo115", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo115(java.lang.Comparable, java.lang.String):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo115(java.lang.Comparable, java.lang.String):::EXIT35",
        decl,
        matcher);

    decl = methodDecls.get(13);
    assertEquals("foo12", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo12(java.lang.Object, java.util.List):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo12(java.lang.Object, java.util.List):::EXIT37",
        decl,
        matcher);

    decl = methodDecls.get(14);
    assertEquals("foo13", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo13(java.lang.Object, java.util.List):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo13(java.lang.Object, java.util.List):::EXIT39",
        decl,
        matcher);

    decl = methodDecls.get(15);
    assertEquals("foo14", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo14(java.lang.Object):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo14(java.lang.Object):::EXIT41", decl, matcher);

    decl = methodDecls.get(16);
    assertEquals("foo15", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo15(java.lang.String):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo15(java.lang.String):::EXIT43", decl, matcher);

    decl = methodDecls.get(17);
    assertEquals("foo16", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo16(java.lang.Object):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo16(java.lang.Object):::EXIT45", decl, matcher);

    decl = methodDecls.get(18);
    assertEquals("foo17", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo17(java.lang.Object[]):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo17(java.lang.Object[]):::EXIT47", decl, matcher);

    decl = methodDecls.get(19);
    assertEquals("foo18", decl.f2.f0.tokenImage);
    checkMatch("daikon.test.GenericTestClass.foo18(java.lang.Object[][]):::ENTER", decl, matcher);
    checkMatch("daikon.test.GenericTestClass.foo18(java.lang.Object[][]):::EXIT49", decl, matcher);

    decl = methodDecls.get(20);
    assertEquals("foo19", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo19(java.lang.Comparable[], java.lang.Object[]):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo19(java.lang.Comparable[], java.lang.Object[]):::EXIT51",
        decl,
        matcher);

    decl = methodDecls.get(21);
    assertEquals("foo20", decl.f2.f0.tokenImage);
    checkMatch(
        "daikon.test.GenericTestClass.foo20"
            + "(java.lang.Comparable[][][], java.lang.Object[][]):::ENTER",
        decl,
        matcher);
    checkMatch(
        "daikon.test.GenericTestClass.foo20"
            + "(java.lang.Comparable[][][], java.lang.Object[][]):::EXIT53",
        decl,
        matcher);

    // Illegal in Java 6
    //
    // decl = methodDecls.get(22);
    // assertEquals("foo1", decl.f2.f0.tokenImage);
    // checkMatch("daikon.test.GenericTestClass.Simple.foo1(java.util.Map.Entry):::ENTER", decl,
    // matcher);
    // checkMatch("daikon.test.GenericTestClass.Simple.foo1(java.util.Map.Entry):::EXIT12", decl,
    // matcher);
    //
    // decl = methodDecls.get(23);
    // assertEquals("foo2", decl.f2.f0.tokenImage);
    // checkMatch("daikon.test.GenericTestClass.Simple.foo2(java.util.Map.Entry):::ENTER", decl,
    // matcher);
    // checkMatch("daikon.test.GenericTestClass.Simple.foo2(java.util.Map.Entry):::EXIT14", decl,
    // matcher);

  }
}
