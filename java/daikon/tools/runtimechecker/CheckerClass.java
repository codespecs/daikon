package daikon.tools.runtimechecker;

import daikon.tools.jtb.Ast;
import jtb.syntaxtree.*;
import org.checkerframework.checker.nullness.qual.NonNull;

/** Represents a class created by the instrumenter to check invariants. */
public class CheckerClass {

  String name;
  StringBuilder code;
  ClassOrInterfaceBody fclassbody;

  public CheckerClass(ClassOrInterfaceBody clazz) {
    this.fclassbody = clazz;

    // The getClassName method may return some $'s and .'s.
    // Since this is going to become a class name, remove them.
    this.name = Ast.getClassName(clazz).replace("$", "_").replace(".", "_") + "_checks";

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
    // Get the package and imports from clazz. We'll include them.
    @SuppressWarnings("nullness") // application invariant: every body is in a compilation unit
    @NonNull CompilationUnit clazzCU =
        (CompilationUnit) Ast.getParent(CompilationUnit.class, clazz);
    NodeOptional no = clazzCU.f0;
    String packageName;
    if (no.present()) {
      packageName = Ast.format(((PackageDeclaration) no.node).f2).trim();
    } else {
      packageName = "";
    }

    String imports = Ast.format(clazzCU.f1);

    code = new StringBuilder();
    if (!packageName.equals("")) {
      code.append("package " + packageName + ";");
    }
    code.append(imports);
    code.append(" public class " + name + "{ ");
  }

  // See getCompilationUnit().
  private boolean alreadyCalled = false;

  /** Must be called only once, when you're done creating this checker. */
  public CompilationUnit getCompilationUnit() {
    if (alreadyCalled) {
      throw new Error("getCompilationUnit should only be called once.");
    }
    alreadyCalled = true;
    code.append("}"); // we're done declaring the class.
    return (CompilationUnit) Ast.create("CompilationUnit", code.toString());
  }

  public String getCheckerClassName() {
    return name;
  }

  public void addDeclaration(StringBuilder decl) {
    code.append(decl);
  }
}
