package daikon.tools.jtb;

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import jtb.*;
import jtb.syntaxtree.*;
import jtb.visitor.*;
import plume.*;

/*>>>
import org.checkerframework.checker.lock.qual.*;
import org.checkerframework.dataflow.qual.*;
*/

/**
 * The wrapped result of parsing a .java source file. The packageName and className arguments can be
 * obtained from root, but they are returned here for convenience.
 */
public class ParseResults {
  public String packageName;

  public String fileName;

  public List<TypeDeclaration> roots = new ArrayList<TypeDeclaration>();

  public CompilationUnit compilationUnit;

  private ParseResults(String packageName, String fileName, CompilationUnit compilationUnit) {
    this.packageName = packageName;
    this.fileName = fileName;
    this.compilationUnit = compilationUnit;
  }

  /*@SideEffectFree*/
  @Override
  public String toString(/*>>>@GuardSatisfied ParseResults this*/) {
    return "package name: " + packageName + ", " + "file name: " + fileName;
  }

  /** If one of the files declares an interfaces, an error will occur. */
  public static List<ParseResults> parse(List<String> javaFileNames) {
    return parse(javaFileNames, false);
  }

  public static List<ParseResults> parse(List<String> javaFileNames, boolean discardComments) {

    List<ParseResults> retval = new ArrayList<ParseResults>();

    for (String javaFileName : javaFileNames) {
      ParseResults results = parse(javaFileName, discardComments);
      retval.add(results);
    }

    return retval;
  }

  public static ParseResults parse(String javaFileName) {
    return parse(javaFileName, false);
  }

  public static ParseResults parse(String javaFileName, boolean discardComments) {

    CompilationUnit compilationUnit;

    System.out.println("Parsing file " + javaFileName);

    File file = new File(javaFileName);
    String fileName = file.getName();
    assert fileName.endsWith(".java")
        : "Found a java-file argument that doesn't end in .java: " + file;

    try {
      Reader input = Files.newBufferedReader(Paths.get(javaFileName), UTF_8);
      JavaParser parser = new JavaParser(input);
      compilationUnit = parser.CompilationUnit();
      input.close();

      // To discard comments, we dump the AST without special
      // tokens, and then we read it again in the same way as
      // before.
      if (discardComments) {
        Writer output = new StringWriter();
        TreeDumper dumper = new TreeDumper(output);
        dumper.printSpecials(false); // Do not print specials <==> discard comments
        compilationUnit.accept(new TreeFormatter());
        compilationUnit.accept(dumper);
        output.close();

        input = new StringReader(output.toString());
        parser = new JavaParser(input);
        compilationUnit = parser.CompilationUnit();
        input.close();
      }

    } catch (Exception e) {
      e.printStackTrace();
      throw new Error(e);
    }

    // Construct the package name.
    String packageNameString;
    // CompilationUnit:
    // f0 -> [ PackageDeclaration() ]
    // f1 -> ( ImportDeclaration() )*
    // f2 -> ( TypeDeclaration() )*
    // f3 -> ( <"\u001a"> )?
    // f4 -> ( <STUFF_TO_IGNORE: ~[]> )?
    // f5 -> <EOF>
    // PackageDeclaration:
    // f0 -> Modifiers()
    // f1 -> "package"
    // f2 -> Name()
    // f3 -> ";"
    NodeOptional packageDeclarationMaybe = compilationUnit.f0;
    if (packageDeclarationMaybe.present()) {
      PackageDeclaration packageDeclaration = (PackageDeclaration) packageDeclarationMaybe.node;
      Name packageName = packageDeclaration.f2;
      StringWriter stringWriter = new StringWriter();
      TreeDumper dumper = new TreeDumper(stringWriter);
      dumper.visit(packageName);
      packageNameString = stringWriter.toString().trim();
    } else {
      packageNameString = "";
    }

    ParseResults results = new ParseResults(packageNameString, fileName, compilationUnit);

    // Find the class name.
    NodeListOptional typeDeclarationMaybe = compilationUnit.f2;
    for (int j = 0; j < typeDeclarationMaybe.size(); j++) {

      TypeDeclaration typeDeclaration = (TypeDeclaration) typeDeclarationMaybe.elementAt(j);

      NodeSequence sequence = (NodeSequence) typeDeclaration.f0.choice;
      NodeChoice nodeChoice = (NodeChoice) sequence.elementAt(1);
      ClassOrInterfaceDeclaration decl = (ClassOrInterfaceDeclaration) nodeChoice.choice;

      //         assert !Ast.isInterface(decl)
      //                           : "Do not give .java files that declare interfaces "
      //                           + "to the instrumenter: " + javaFileName;

      results.roots.add(typeDeclaration);
    }

    return results;
  }
}
