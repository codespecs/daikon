package daikon.tools.jtb;

import jtb.*;
import jtb.visitor.*;
import jtb.syntaxtree.*;

import java.util.*;
import java.io.*;

import utilMDE.*;

/**
 * The wrapped result of parsing a .java source file. The packageName and
 * className arguments can be obtained from root, but they are returned here
 * for convenience.
 */
public class ParseResults {
  public String packageName;

  public String fileName;

  public List<TypeDeclaration> roots = new ArrayList<TypeDeclaration>();

  public CompilationUnit compilationUnit;

  public String toString() {
    return "package name: " + packageName + ", " + "file name: "
      + fileName;
  }

  /**
   * If one of the files declares an interfaces, an error will occur.
   */
  public static List<ParseResults> parse(List<String> javaFileNames) {

    List<ParseResults> retval = new ArrayList<ParseResults>();

    for (Iterator i = javaFileNames.iterator(); i.hasNext();) {
      String javaFileName = (String) i.next();
      ParseResults results = new ParseResults();

      CompilationUnit compilationUnit = null;

      System.out.println("Parsing file " + javaFileName);

      File file = new File(javaFileName);
      String name = file.getName();
      Assert.assertTrue(name.endsWith(".java"),
                        "Found a java-file argument that doesn't end in .java: "
                        + file);

      results.fileName = name;

      try {
        Reader input = new FileReader(javaFileName);
        JavaParser parser = new JavaParser(input);
        compilationUnit = parser.CompilationUnit();
        input.close();
      } catch (Exception e) {
        e.printStackTrace();
        throw new Error(e);
      }

      results.compilationUnit = compilationUnit;

      // Construct the package name.
      NodeOptional packageDeclarationMaybe = compilationUnit.f0;
      if (packageDeclarationMaybe.present()) {
        PackageDeclaration packageDeclaration =
          (PackageDeclaration) packageDeclarationMaybe.node;
        Name packageName = packageDeclaration.f1;
        StringWriter stringWriter = new StringWriter();
        TreeDumper dumper = new TreeDumper(stringWriter);
        dumper.visit(packageName);
        results.packageName = stringWriter.toString().trim();
      } else {
        results.packageName = "";
      }

      // Find the class name.
      NodeListOptional typeDeclarationMaybe = compilationUnit.f2;
      for (int j = 0 ; j < typeDeclarationMaybe.size() ; j++) {

        TypeDeclaration typeDeclaration =
          (TypeDeclaration) typeDeclarationMaybe.elementAt(j);

        NodeSequence sequence = (NodeSequence)typeDeclaration.f0.choice;
        NodeChoice nodeChoice = (NodeChoice)sequence.elementAt(1);
        ClassOrInterfaceDeclaration decl =
          (ClassOrInterfaceDeclaration)nodeChoice.choice;

//         Assert.assertTrue(!Ast.isInterface(decl),
//                           "Do not give .java files that declare interfaces "
//                           + "to the instrumenter: " + javaFileName);

        results.roots.add(typeDeclaration);
      }

      // Add to the list of ParseResults that we'll return.
      retval.add(results);
    }
    return retval;
  }
}
