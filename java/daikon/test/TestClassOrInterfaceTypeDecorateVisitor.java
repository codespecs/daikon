package daikon.test;

import static org.junit.Assert.assertEquals;

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
import org.plumelib.util.StringsPlume;

public final class TestClassOrInterfaceTypeDecorateVisitor {

  public static class UngenerifiedTypeCollector extends DepthFirstVisitor {
    // These two lists have the same length.
    List<ClassOrInterfaceType> generifieds = new ArrayList<>();
    List<ClassOrInterfaceType> ungenerifieds = new ArrayList<>();

    @Override
    public void visit(ClassOrInterfaceType n) {
      generifieds.add(n);
      ungenerifieds.add(n.unGenerifiedVersionOfThis);
    }

    /**
     * A printed representation of the results.
     *
     * @return a printed representation of the results
     */
    public String collectionResults() {
      StringBuilder b = new StringBuilder();
      b.append("Collection results:");
      b.append(System.lineSeparator());
      for (int i = 0; i < generifieds.size(); i++) {
        MethodDeclaration m =
            (MethodDeclaration) Ast.getParent(MethodDeclaration.class, generifieds.get(i));
        if (m != null) {
          b.append("Method: ");
          m.f0.accept(new TreeFormatter());
          b.append(Ast.format(m.f0));
          m.f1.accept(new TreeFormatter());
          b.append(Ast.format(m.f1));
          m.f2.accept(new TreeFormatter());
          b.append(Ast.format(m.f2));
          b.append(System.lineSeparator());
        }
        generifieds.get(i).accept(new TreeFormatter());
        b.append("  " + Ast.format(generifieds.get(i)));
        b.append("  -->");
        ungenerifieds.get(i).accept(new TreeFormatter());
        b.append("  " + Ast.format(ungenerifieds.get(i)));
        b.append(System.lineSeparator());
      }
      return b.toString();
    }
  }

  @Test
  public void testTheVisitor() {

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

    UngenerifiedTypeCollector ungenerifiedCollector = new UngenerifiedTypeCollector();
    compilationUnit.accept(new ClassOrInterfaceTypeDecorateVisitor());
    compilationUnit.accept(ungenerifiedCollector);

    /*
     for (int ii = 0; ii < result.length(); ii++) {
      if (result.charAt(ii) !=  expected.charAt(ii)) {
        System.out.printf("diff at offset %d: '%c' - '%c'%n", ii,
                           result.charAt(ii), expected.charAt(ii));
        System.out.printf("last:%n%s%n%s%n", result.substring (ii-50, ii+2),
                           expected.substring (ii-50, ii+2));
        break;
      }
    }
    */

    String result = ungenerifiedCollector.collectionResults().trim();
    String[] result_arr = StringsPlume.splitLines(result);

    // FilesPlume.writeFile(new File("expected.txt"), expected);
    // FilesPlume.writeFile(new File("result.txt"), result);

    assertEquals(expectedAnswerLines.length, result_arr.length);
    for (int ii = 0; ii < expectedAnswerLines.length; ii++) {
      assertEquals(expectedAnswerLines[ii], result_arr[ii]);
    }
    /*
    assertEquals(expectedAnswerBuffer.toString().trim(), ungenerifiedCollector.collectionResults().trim());
      */
  }

  /** The expected results, as a collection of lines. */
  private static String[] expectedAnswerLines =
      new String[] {
        "Collection results:",
        "  String  -->  String",
        "  java.lang.Object  -->  java.lang.Object",
        "Method: Listfoo1()",
        "  List  -->  List",
        "Method: List<String>foo2()",
        "  List<String>  -->  List",
        "Method: Ufoo3()",
        "  U  -->  Object",
        "Method: <D extends Comparable >List<String>foo4()",
        "  Comparable  -->  Comparable",
        "Method: <D extends Comparable >List<String>foo4()",
        "  List<String>  -->  List",
        "Method: <E extends java.lang.Object >List<U>foo5()",
        "  java.lang.Object  -->  java.lang.Object",
        "Method: <E extends java.lang.Object >List<U>foo5()",
        "  List<U>  -->  List",
        "Method: <F >List<String>foo55()",
        "  List<String>  -->  List",
        "Method: Listfoo6(List x)",
        "  List  -->  List",
        "Method: Listfoo6(List x)",
        "  List  -->  List",
        "Method: Listfoo7(List<A> x)",
        "  List  -->  List",
        "Method: Listfoo7(List<A> x)",
        "  List<A>  -->  List",
        "Method: Listfoo8(A x)",
        "  List  -->  List",
        "Method: Listfoo8(A x)",
        "  A  -->  Object",
        "Method: Listfoo9(B x)",
        "  List  -->  List",
        "Method: Listfoo9(B x)",
        "  B  -->  String",
        "Method: Listfoo10(C x)",
        "  List  -->  List",
        "Method: Listfoo10(C x)",
        "  C  -->  java.lang.Object",
        "Method: <G extends Comparable >List<U>foo11(G x, C y)",
        "  Comparable  -->  Comparable",
        "Method: <G extends Comparable >List<U>foo11(G x, C y)",
        "  List<U>  -->  List",
        "Method: <G extends Comparable >List<U>foo11(G x, C y)",
        "  G  -->  Comparable",
        "Method: <G extends Comparable >List<U>foo11(G x, C y)",
        "  C  -->  java.lang.Object",
        "Method: <C extends Comparable >List<U>foo115(C x, B y)",
        "  Comparable  -->  Comparable",
        "Method: <C extends Comparable >List<U>foo115(C x, B y)",
        "  List<U>  -->  List",
        "Method: <C extends Comparable >List<U>foo115(C x, B y)",
        "  C  -->  Comparable",
        "Method: <C extends Comparable >List<U>foo115(C x, B y)",
        "  B  -->  String",
        "Method: <G extends Comparable >List<String>foo12(A x, List<B> y)",
        "  Comparable  -->  Comparable",
        "Method: <G extends Comparable >List<String>foo12(A x, List<B> y)",
        "  List<String>  -->  List",
        "Method: <G extends Comparable >List<String>foo12(A x, List<B> y)",
        "  A  -->  Object",
        "Method: <G extends Comparable >List<String>foo12(A x, List<B> y)",
        "  List<B>  -->  List",
        "Method: <G extends Comparable >List<String>foo13(A x, List<U> y)",
        "  Comparable  -->  Comparable",
        "Method: <G extends Comparable >List<String>foo13(A x, List<U> y)",
        "  List<String>  -->  List",
        "Method: <G extends Comparable >List<String>foo13(A x, List<U> y)",
        "  A  -->  Object",
        "Method: <G extends Comparable >List<String>foo13(A x, List<U> y)",
        "  List<U>  -->  List",
        "Method: <H extends java.lang.Object >List<String>foo14(H x)",
        "  java.lang.Object  -->  java.lang.Object",
        "Method: <H extends java.lang.Object >List<String>foo14(H x)",
        "  List<String>  -->  List",
        "Method: <H extends java.lang.Object >List<String>foo14(H x)",
        "  H  -->  java.lang.Object",
        "Method: <H extends java.lang.Object >List<U>foo15(B x)",
        "  java.lang.Object  -->  java.lang.Object",
        "Method: <H extends java.lang.Object >List<U>foo15(B x)",
        "  List<U>  -->  List",
        "Method: <H extends java.lang.Object >List<U>foo15(B x)",
        "  B  -->  String",
        "Method: <I >List<String>foo16(I x)",
        "  List<String>  -->  List",
        "Method: <I >List<String>foo16(I x)",
        "  I  -->  Object",
        "Method: <I >List<String>foo17(I[] x)",
        "  List<String>  -->  List",
        "Method: <I >List<String>foo17(I[] x)",
        "  I  -->  Object",
        "Method: <I >List<String>foo18(I[][] x)",
        "  List<String>  -->  List",
        "Method: <I >List<String>foo18(I[][] x)",
        "  I  -->  Object",
        "Method: <G extends Comparable >List<U>foo19(G[] x, C[] y)",
        "  Comparable  -->  Comparable",
        "Method: <G extends Comparable >List<U>foo19(G[] x, C[] y)",
        "  List<U>  -->  List",
        "Method: <G extends Comparable >List<U>foo19(G[] x, C[] y)",
        "  G  -->  Comparable",
        "Method: <G extends Comparable >List<U>foo19(G[] x, C[] y)",
        "  C  -->  java.lang.Object",
        "Method: // Ugh! But this is legal.//",
        "/* */",
        "List[]foo20(Comparable[][] x[], Object[] y[])[]",
        "  // Ugh! But this is legal.//",
        "/* */",
        "List  -->  // Ugh! But this is legal.//",
        "/* */",
        "List",
        "Method: // Ugh! But this is legal.//",
        "/* */",
        "List[]foo20(Comparable[][] x[], Object[] y[])[]",
        "  Comparable  -->  Comparable",
        "Method: // Ugh! But this is legal.//",
        "/* */",
        "List[]foo20(Comparable[][] x[], Object[] y[])[]",
        "  Object  -->  Object",
        // This is illegal in Java 6.
        // "  Map  -->  Map",
        // "  U.Entry  -->  Map.Entry",
        // "Method: voidfoo1(V x)",
        // "  V  -->  Map.Entry",
        // "Method: voidfoo2(U.Entry x)",
        // "  U.Entry  -->  Map.Entry",
      };
}
